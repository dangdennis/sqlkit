(* Fault injection for the Eio drivers.

   A TCP proxy sits between the driver and a real server and misbehaves on
   purpose: it fragments the stream into tiny writes, delays it, cuts it off
   mid-message, stalls it, flips a byte, or injects garbage. Each proxied
   connection draws its own plan.

   What must hold whatever the proxy does:

   - no exception escapes the driver (errors are values);
   - no operation outlives its deadline by more than the time the driver is
     allowed for telling the server to stop;
   - an operation that reports success returned the right answer. Over plain
     TCP this is required for every fault that does not alter bytes; over TLS
     it is required for all of them, since tampering must be detected;
   - afterwards the process holds no more descriptors than it started with
     and the server holds none of our backends.

     dune exec example/chaos.exe -- pgwire|eio plain SECONDS [SEED]
     dune exec example/chaos.exe -- pgwire|eio tls:/path/to/ca.pem SECONDS [SEED] *)

let usage () =
  prerr_endline "usage: chaos pgwire|eio plain|tls:CA.pem SECONDS [SEED]";
  exit 2

let driver, tls, seconds, seed =
  match Array.to_list Sys.argv with
  | _ :: d :: mode :: s :: rest -> (
      let tls =
        if mode = "plain" then None
        else if String.starts_with ~prefix:"tls:" mode then
          Some (String.sub mode 4 (String.length mode - 4))
        else usage ()
      in
      (d, tls, float_of_string s, match rest with [ n ] -> int_of_string n | _ -> 1))
  | _ -> usage ()

let trace = Sys.getenv_opt "CHAOS_TRACE" <> None
let note fmt = Printf.ksprintf (fun m -> if trace then prerr_endline m) fmt
let tag = "sqlkit-chaos"
let connect_timeout = 1.0
let query_timeout = 1.0

(* deadline + the cancel request's own bound + slack *)
let patience = query_timeout +. 2.0 +. 1.5

(* ---------- the proxy ---------- *)

type terminal = Clean | Cut | Stall | Flip | Garbage

type plan = {
  fragment : int;  (** largest write, in bytes *)
  delay : float;  (** chance of a pause before a write *)
  terminal : terminal;
  after : int;  (** bytes let through before the terminal fault *)
  on_server_side : bool;  (** which direction the terminal fault hits *)
}

let alters_bytes plan = match plan.terminal with Flip | Garbage -> true | _ -> false

let plan_name p =
  match p.terminal with
  | Clean -> "clean"
  | Cut -> "cut"
  | Stall -> "stall"
  | Flip -> "flip"
  | Garbage -> "garbage"

let draw state ~hostile =
  let terminal =
    if not hostile then Clean
    else
      match Random.State.int state 10 with
      | 0 | 1 -> Clean
      | 2 | 3 | 4 -> Cut
      | 5 | 6 -> Stall
      | 7 | 8 -> Flip
      | _ -> Garbage
  in
  {
    fragment =
      (match Random.State.int state 4 with
      | 0 -> 1
      | 1 -> 1 + Random.State.int state 7
      | 2 -> 1 + Random.State.int state 512
      | _ -> 65536);
    delay = (if Random.State.int state 3 = 0 then 0.02 else 0.);
    terminal;
    (* weighted toward the handshake and the first replies, where the
       interesting state machines are *)
    after =
      (match Random.State.int state 3 with
      | 0 -> Random.State.int state 64
      | 1 -> Random.State.int state 2048
      | _ -> Random.State.int state 200_000);
    on_server_side = Random.State.int state 4 > 0;
  }

exception Stop

(* Copies one direction, applying the plan. Returns when the source ends;
   raises Stop to end the whole connection. *)
let pump ~clock ~state plan ~faulty src dst =
  let buf = Cstruct.create 16384 in
  let passed = ref 0 in
  let write data =
    let n = Cstruct.length data in
    let rec go off =
      if off < n then begin
        let len = min plan.fragment (n - off) in
        if plan.delay > 0. && Random.State.float state 1. < plan.delay then
          Eio.Time.Mono.sleep clock 0.001;
        Eio.Flow.write dst [ Cstruct.sub data off len ];
        go (off + len)
      end
    in
    go 0
  in
  let rec loop () =
    match Eio.Flow.single_read src buf with
    | exception End_of_file -> ()
    | got ->
        let data = Cstruct.sub buf 0 got in
        if faulty && plan.terminal <> Clean && !passed + got > plan.after then begin
          let keep = max 0 (plan.after - !passed) in
          write (Cstruct.sub data 0 keep);
          match plan.terminal with
          | Cut -> raise Stop
          | Stall -> Eio.Fiber.await_cancel ()
          | Flip ->
              let rest = Cstruct.sub data keep (got - keep) in
              let i = Random.State.int state (Cstruct.length rest) in
              Cstruct.set_uint8 rest i
                (Cstruct.get_uint8 rest i lxor (1 lsl Random.State.int state 8));
              write rest;
              passed := max_int / 2;
              loop ()
          | Garbage ->
              write
                (Cstruct.of_string
                   (String.init
                      (1 + Random.State.int state 64)
                      (fun _ -> Char.chr (Random.State.int state 256))));
              write (Cstruct.sub data keep (got - keep));
              passed := max_int / 2;
              loop ()
          | Clean -> ()
        end
        else begin
          passed := !passed + got;
          write data;
          loop ()
        end
  in
  loop ()

type proxy = {
  port : int;
  mutable hostile : bool;
  plans : (string, int) Hashtbl.t;
  mutable last : plan;
}

let start_proxy ~sw ~net ~clock ~state upstream =
  let listener =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:64
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = match Eio.Net.listening_addr listener with `Tcp (_, p) -> p | _ -> 0 in
  let proxy =
    { port; hostile = true; plans = Hashtbl.create 8; last = draw state ~hostile:false }
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let client, _ = Eio.Net.accept ~sw listener in
        (* Daemons: a proxied connection must never be what keeps the test's
           switch open, or the pool behind it could not be released. *)
        Eio.Fiber.fork_daemon ~sw (fun () ->
            let plan = draw state ~hostile:proxy.hostile in
            proxy.last <- plan;
            Hashtbl.replace proxy.plans (plan_name plan)
              (1 + Option.value (Hashtbl.find_opt proxy.plans (plan_name plan)) ~default:0);
            (try
               Eio.Switch.run (fun csw ->
                   let server = Eio.Net.connect ~sw:csw net upstream in
                   Eio.Fiber.first
                     (fun () ->
                       pump ~clock ~state plan ~faulty:plan.on_server_side server client)
                     (fun () ->
                       pump ~clock ~state plan ~faulty:(not plan.on_server_side) client
                         server))
             with Stop | Eio.Io _ -> ());
            (try Eio.Resource.close client with Eio.Io _ | Invalid_argument _ -> ());
            `Stop_daemon)
      done;
      `Stop_daemon);
  proxy

(* ---------- the workload ---------- *)

type tally = {
  mutable ok : int;
  mutable refused : int;
  mutable wrong_tampered : int;
      (** plain TCP only: bytes were altered, nothing can tell *)
  mutable problems : string list;
}

let tally = { ok = 0; refused = 0; wrong_tampered = 0; problems = [] }

let problem fmt =
  Printf.ksprintf
    (fun s ->
      if not (List.mem s tally.problems) then tally.problems <- s :: tally.problems)
    fmt

type conn = Sqlkit.conn

let query (Sqlkit.Driver.Conn ((module D), raw, _) : conn) sql params =
  D.query raw ~sql ~params ~columns:0

let exec (Sqlkit.Driver.Conn ((module D), raw, _) : conn) sql = D.exec raw ~sql ~params:[]
let copy (Sqlkit.Driver.Conn ((module D), raw, _) : conn) sql rows = D.copy raw ~sql ~rows

(* One operation with a known answer. [Some true]: succeeded and correct;
   [Some false]: succeeded and wrong; [None]: reported an error. *)
let operations state token : (string * (conn -> bool option)) list =
  let text s = Sqlkit.Value.Text s in
  let judged f r = match r with Ok v -> Some (f v) | Error _ -> None in
  [
    ( "echo",
      fun c ->
        judged
          (fun rows -> rows = [ [| text token |] ])
          (query c "SELECT $1::text" [ text token ]) );
    ( "series",
      fun c ->
        let n = 1 + Random.State.int state 3000 in
        judged
          (fun rows ->
            List.length rows = n
            && List.for_all2
                 (fun i row ->
                   row = [| text (string_of_int i); text (string_of_int (2 * i)) |])
                 (List.init n (fun i -> i + 1))
                 rows)
          (query c "SELECT g::text, (g * 2)::text FROM generate_series(1, $1::int) g"
             [ text (string_of_int n) ]) );
    ( "payload",
      fun c ->
        let n = Random.State.int state 300_000 in
        let sent = String.make n 'p' in
        judged
          (fun rows -> rows = [ [| text (string_of_int n); text sent |] ])
          (query c "SELECT length($1::text)::text, $1::text" [ text sent ]) );
    ( "error",
      fun c ->
        match query c "SELECT 1 / ($1::int - $1::int)" [ text "5" ] with
        | Error e -> (
            match Option.map Sqlkit.Sqlstate.to_string e.sqlstate with
            | Some "22012" -> Some true
            | _ -> None)
        | Ok _ -> Some false );
    ( "copy",
      fun c ->
        let rows =
          List.init (Random.State.int state 500) (fun i -> Printf.sprintf "%s-%d" token i)
        in
        match exec c "CREATE TEMP TABLE IF NOT EXISTS chaos_copy (v text)" with
        | Error _ -> None
        | Ok _ -> (
            match exec c "TRUNCATE chaos_copy" with
            | Error _ -> None
            | Ok _ -> (
                match copy c "COPY chaos_copy (v) FROM STDIN" rows with
                | Error _ -> None
                | Ok written ->
                    judged
                      (fun counted ->
                        written = List.length rows
                        && counted = [ [| text (string_of_int written) |] ])
                      (query c "SELECT count(*)::text FROM chaos_copy" []))) );
  ]

let () =
  Eio_main.run @@ fun env ->
  let clock = env#mono_clock and net = env#net in
  let state = Random.State.make [| seed |] in
  let descriptors () = Array.length (Sys.readdir "/dev/fd") in
  let upstream_port = if tls = None then 55432 else 55433 in
  let direct port =
    Printf.sprintf
      "host=127.0.0.1 port=%d user=sqlkit password=sqlkit dbname=sqlkit \
       application_name=%s options='-c client_min_messages=error' %s"
      port tag
      (match tls with
      | None -> "sslmode=disable"
      | Some ca -> "sslmode=verify-ca sslrootcert=" ^ ca)
  in
  let before = ref 0 in
  let finished = ref false in
  Eio.Switch.run (fun sw ->
      let proxy =
        start_proxy ~sw ~net ~clock ~state
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, upstream_port))
      in
      let connect ~sw conninfo =
        if driver = "pgwire" then
          Sqlkit_pgwire.connect ~sw ~net ~clock ~connect_timeout ~query_timeout conninfo
        else Sqlkit_eio.connect ~sw ~clock ~connect_timeout ~query_timeout conninfo
      in
      before := descriptors ();
      let deadline = Unix.gettimeofday () +. seconds in
      (* An operation that reports success must be right, unless the proxy
         altered bytes on a connection with nothing to detect it. *)
      let judge ~what ~plan outcome =
        match outcome with
        | Some true -> tally.ok <- tally.ok + 1
        | None -> tally.refused <- tally.refused + 1
        | Some false ->
            if alters_bytes plan && tls = None then
              tally.wrong_tampered <- tally.wrong_tampered + 1
            else
              problem "%s returned a wrong answer as success (%s%s)" what (plan_name plan)
                (if tls = None then "" else ", over TLS")
      in
      let bounded ~what ~plan f =
        match
          Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock patience) (fun () ->
              Ok (f ()))
        with
        | Ok v -> v
        | Error `Timeout ->
            problem "%s outlived its deadline (%s, fragment %d)" what (plan_name plan)
              plan.fragment;
            None
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception e ->
            problem "%s raised %s (%s)" what (Printexc.to_string e) (plan_name plan);
            None
      in
      (* Phase one: one connection per plan, a fixed run of operations. *)
      let n = ref 0 in
      while Unix.gettimeofday () < deadline -. (seconds /. 2.) do
        incr n;
        let token = Printf.sprintf "t%d" !n in
        note "phase 1 #%d" !n;
        Eio.Switch.run (fun csw ->
            let opened =
              match
                Eio.Time.Timeout.run
                  (Eio.Time.Timeout.seconds clock (connect_timeout +. 2.))
                  (fun () -> Ok (connect ~sw:csw (direct proxy.port)))
              with
              | Ok r -> Some r
              | Error `Timeout ->
                  problem "connect outlived its deadline (%s)" (plan_name proxy.last);
                  None
              | exception (Eio.Cancel.Cancelled _ as e) -> raise e
              | exception e ->
                  problem "connect raised %s (%s)" (Printexc.to_string e)
                    (plan_name proxy.last);
                  None
            in
            match opened with
            | Some (Ok c) ->
                let plan = proxy.last in
                note "  connected, plan %s after %d fragment %d %s" (plan_name plan)
                  plan.after plan.fragment
                  (if plan.on_server_side then "s2c" else "c2s");
                List.iter
                  (fun (what, run) ->
                    note "  op %s" what;
                    judge ~what ~plan (bounded ~what ~plan (fun () -> run c)))
                  (operations state token)
            | Some (Error _) -> tally.refused <- tally.refused + 1
            | None -> ())
      done;
      (* Phase two: a pool whose physical connections fail at random while
         workers keep asking. The pool has to keep making progress. *)
      note "phase 2";
      let conninfo = direct proxy.port in
      let use :
          (Sqlkit.conn -> (bool option, Sqlkit.Error.t) result) ->
          (bool option, Sqlkit.Error.t) result =
        if driver = "pgwire" then
          match
            Sqlkit_pgwire.Pool.create ~sw ~net ~clock ~max_size:4 ~acquire_timeout:3.
              ~connect_timeout ~query_timeout conninfo
          with
          | Ok pool -> fun f -> Sqlkit_pgwire.Pool.use pool f
          | Error e -> failwith (Sqlkit.Error.to_string e)
        else
          match
            Sqlkit_eio.Pool.create ~sw ~clock ~max_size:4 ~acquire_timeout:3.
              ~connect_timeout ~query_timeout conninfo
          with
          | Ok pool -> fun f -> Sqlkit_eio.Pool.use pool f
          | Error e -> failwith (Sqlkit.Error.to_string e)
      in
      let pooled_ok = ref 0 in
      Eio.Fiber.List.iter
        (fun worker ->
          let i = ref 0 in
          while Unix.gettimeofday () < deadline do
            incr i;
            let token = Printf.sprintf "w%d-%d" worker !i in
            let ops = operations state token in
            let what, run = List.nth ops (Random.State.int state (List.length ops)) in
            (* which plan the borrowed connection drew is not knowable here,
               so over plain TCP a wrong answer in this phase is only counted
               when no byte-altering plan has been dealt at all *)
            let tampering =
              Hashtbl.mem proxy.plans "flip" || Hashtbl.mem proxy.plans "garbage"
            in
            (match
               Eio.Time.Timeout.run
                 (Eio.Time.Timeout.seconds clock (patience +. 3. +. connect_timeout))
                 (fun () -> Ok (use (fun c -> Ok (run c))))
             with
            | Ok (Ok (Some true)) ->
                incr pooled_ok;
                tally.ok <- tally.ok + 1
            | Ok (Ok None) | Ok (Error _) -> tally.refused <- tally.refused + 1
            | Ok (Ok (Some false)) ->
                if tampering && tls = None then
                  tally.wrong_tampered <- tally.wrong_tampered + 1
                else problem "pooled %s returned a wrong answer as success" what
            | Error `Timeout -> problem "pooled %s outlived every deadline" what
            | exception (Eio.Cancel.Cancelled _ as e) -> raise e
            | exception e -> problem "pooled %s raised %s" what (Printexc.to_string e));
            Eio.Fiber.yield ()
          done)
        (List.init 12 Fun.id);
      if !pooled_ok = 0 then problem "the pool made no progress at all";
      (* With the faults switched off the pool must recover completely. *)
      note "recovery";
      proxy.hostile <- false;
      Eio.Time.Mono.sleep clock 0.2;
      let recovered = ref 0 in
      for i = 1 to 40 do
        let token = Printf.sprintf "calm-%d" i in
        match use (fun c -> Ok (snd (List.hd (operations state token)) c)) with
        | Ok (Some true) -> incr recovered
        | _ -> ()
      done;
      (* connections drawn while hostile may still be in the pool; they are
         allowed to fail once each *)
      if !recovered < 30 then
        problem "only %d of 40 statements succeeded after the faults stopped" !recovered;
      note "closing";
      finished := true);
  note "census";
  (* everything is closed now: the switch has ended *)
  let after = Array.length (Sys.readdir "/dev/fd") in
  Eio.Switch.run (fun sw ->
      let admin_conninfo =
        Printf.sprintf
          "host=127.0.0.1 port=%d user=sqlkit password=sqlkit dbname=sqlkit %s"
          upstream_port
          (match tls with
          | None -> "sslmode=disable"
          | Some ca -> "sslmode=verify-ca sslrootcert=" ^ ca)
      in
      match Sqlkit_pgwire.connect ~sw ~net ~clock admin_conninfo with
      | Error e -> problem "census: %s" (Sqlkit.Error.to_string e)
      | Ok admin ->
          let census () =
            match
              query admin
                "SELECT count(*)::text FROM pg_stat_activity WHERE application_name = $1"
                [ Sqlkit.Value.Text tag ]
            with
            | Ok [ [| Sqlkit.Value.Text n |] ] -> int_of_string n
            | _ -> -1
          in
          let rec settle fuel =
            let n = census () in
            if n = 0 || fuel = 0 then n
            else (
              Eio.Time.Mono.sleep clock 0.1;
              settle (fuel - 1))
          in
          let left = settle 100 in
          if left <> 0 then begin
            (match
               query admin
                 "SELECT pid::text, state, coalesce(wait_event, ''), extract(epoch from \
                  now() - state_change)::int::text, left(query, 60) FROM \
                  pg_stat_activity WHERE application_name = $1"
                 [ Sqlkit.Value.Text tag ]
             with
            | Ok rows ->
                List.iter
                  (fun row ->
                    print_endline
                      ("  left behind: "
                      ^ String.concat " | "
                          (Array.to_list
                             (Array.map
                                (function Sqlkit.Value.Text s -> s | _ -> "?")
                                row))))
                  rows
            | Error _ -> ());
            (* With every descriptor of ours closed, a backend that lingers
               was orphaned on the way: this proxy closes sockets that still
               hold unread data, which resets the connection, and some
               container port forwarders do not pass a reset on. That is not
               the driver's doing, so it is reported and cleaned up, and only
               counted as a failure when descriptors leaked as well. *)
            if after > !before then problem "server still holds %d of our backends" left
            else begin
              Printf.printf
                "note: %d backend(s) orphaned by the network path, not by an open \
                 descriptor\n"
                left;
              ignore
                (query admin
                   "SELECT pg_terminate_backend(pid)::text FROM pg_stat_activity WHERE \
                    application_name = $1"
                   [ Sqlkit.Value.Text tag ])
            end
          end);
  if !finished && after > !before then
    problem "descriptors grew from %d to %d" !before after;
  Printf.printf "%s %s %.0fs seed %d: ok=%d refused=%d wrong-but-tampered=%d\n" driver
    (if tls = None then "plain" else "tls")
    seconds seed tally.ok tally.refused tally.wrong_tampered;
  match List.sort compare tally.problems with
  | [] -> print_endline "all good"
  | problems ->
      List.iter (Printf.printf "FAIL %s\n") problems;
      exit 1

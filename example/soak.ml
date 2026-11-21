(* Soak for the Eio drivers: many fibers on a small pool, each picking at
   random between ordinary statements, statements that fail, large payloads,
   transactions, COPY, statements cancelled part-way, statements that outlive
   their deadline, and backends terminated from outside.

   Three things must hold however the dice fall: every answer belongs to the
   question that asked for it, the pool keeps working, and once the dust
   settles the server holds no more of our backends than the pool allows and
   none of them is still busy.

     dune exec example/soak.exe -- [eio|pgwire] [SECONDS] [SEED] *)

open Generated

let driver, seconds, seed =
  match Sys.argv with
  | [| _; d |] -> (d, 10., 1)
  | [| _; d; s |] -> (d, float_of_string s, 1)
  | [| _; d; s; seed |] -> (d, float_of_string s, int_of_string seed)
  | _ -> ("eio", 10., 1)

type pool = {
  use : 'a. (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) -> ('a, Sqlkit.Error.t) result;
}

let pool_size = 6
let workers = 24
let tag = "sqlkit-soak"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock and net = env#net in
  (* Works for a URI with or without a query, and for key=value. Behind
     PgBouncer the options parameter needs ignore_startup_parameters. *)
  let conninfo =
    let base = Sqlkit_eio.conninfo_of_env () in
    if String.starts_with ~prefix:"postgres" base && String.contains base ':' then
      Printf.sprintf
        "%s%sapplication_name=%s&options=-c%%20client_min_messages%%3Dwarning" base
        (if String.contains base '?' then "&" else "?")
        tag
    else
      Printf.sprintf "%s application_name=%s options='-c client_min_messages=warning'"
        base tag
  in
  let limits = { Sqlkit.Limits.default with statement_timeout_ms = 2000 } in
  let { use }, admin =
    let fail e = failwith (Sqlkit.Error.to_string e) in
    let ok = function Ok v -> v | Error e -> fail e in
    if driver = "pgwire" then
      let pool =
        ok
          (Sqlkit_pgwire.Pool.create ~sw ~net ~clock ~max_size:pool_size
             ~query_timeout:0.5 ~limits conninfo)
      in
      ( { use = (fun f -> Sqlkit_pgwire.Pool.use pool f) },
        ok (Sqlkit_pgwire.connect ~sw ~net ~clock (Sqlkit_pgwire.conninfo_of_env ())) )
    else
      let pool =
        ok
          (Sqlkit_eio.Pool.create ~sw ~clock ~max_size:pool_size ~query_timeout:0.5
             ~limits conninfo)
      in
      ( { use = (fun f -> Sqlkit_eio.Pool.use pool f) },
        ok (Sqlkit_eio.connect ~sw ~clock (Sqlkit_eio.conninfo_of_env ())) )
  in
  let raw conn sql params =
    let (Sqlkit.Driver.Conn ((module D), c, _)) = conn in
    D.query c ~sql ~params ~columns:0
  in
  let admin_lock = Eio.Mutex.create () in
  let admin_rows sql params =
    Eio.Mutex.use_rw ~protect:true admin_lock (fun () -> raw admin sql params)
  in
  let state = Random.State.make [| seed |] in
  let counts = Hashtbl.create 16 in
  let bump what =
    Hashtbl.replace counts what
      (1 + Option.value (Hashtbl.find_opt counts what) ~default:0)
  in
  let wrong = ref [] in
  let expect what ok = if not ok then wrong := what :: !wrong in
  let deadline = Unix.gettimeofday () +. seconds in
  let step worker n =
    let token = Printf.sprintf "%d-%d" worker n in
    match Random.State.int state 100 with
    | r when r < 40 -> (
        bump "echo";
        (* the answer must be ours: a mixed-up pipeline would hand back
           somebody else's token *)
        match
          use (fun c ->
              Ok (raw c "SELECT $1::text, pg_backend_pid()::text" [ Text token ]))
        with
        | Ok (Ok [ [| Text t; _ |] ]) -> expect ("echo " ^ token) (t = token)
        | Ok (Ok _) -> expect ("echo shape " ^ token) false
        | Ok (Error _) | Error _ -> bump "echo refused")
    | r when r < 50 -> (
        bump "payload";
        let size = Random.State.int state 300_000 in
        match use (fun c -> Db.payload_size c ~payload:(String.make size 'p')) with
        | Ok r -> expect "payload size" (r.size = size)
        | Error _ -> bump "payload refused")
    | r when r < 60 -> (
        bump "error";
        match use (fun c -> Ok (raw c "SELECT 1/($1::int - $1::int)" [ Text "7" ])) with
        | Ok (Error e) -> (
            (* its own error, unless the backend was terminated underneath it *)
            match Option.map Sqlkit.Sqlstate.to_string e.sqlstate with
            | Some "22012" -> ()
            | Some "57P01" | None -> bump "error refused"
            | Some other -> expect ("division by zero reported as " ^ other) false)
        | Ok (Ok _) -> expect "division by zero succeeded" false
        | Error _ -> bump "error refused")
    | r when r < 70 -> (
        bump "transaction";
        (* a failed statement inside a transaction, rolled back by the runtime *)
        let refused (e : Sqlkit.Driver.error) =
          Sqlkit.Error.Invalid_argument { operation = "soak"; message = e.message }
        in
        let fails = Random.State.bool state in
        let r =
          use (fun c ->
              Sqlkit.transaction c (fun tx ->
                  let sql =
                    if fails then "SELECT (1/($1::int - $1::int))::text"
                    else "SELECT $1::text"
                  in
                  let arg = if fails then "3" else token in
                  Result.map_error refused (raw tx sql [ Text arg ])))
        in
        match r with
        | Ok [ [| Text t |] ] -> expect "transaction echo" ((not fails) && t = token)
        | Ok _ -> expect "transaction shape" false
        | Error _ -> if not fails then bump "transaction refused")
    | r when r < 78 -> (
        bump "copy";
        let (rows : string list) =
          List.init (Random.State.int state 400) (fun i -> Printf.sprintf "%s-%d" token i)
        in
        let r =
          use (fun c ->
              let (Sqlkit.Driver.Conn ((module D), raw_c, _)) = c in
              ignore
                (D.exec raw_c ~sql:"CREATE TEMP TABLE IF NOT EXISTS soak_copy (v text)"
                   ~params:[]);
              ignore (D.exec raw_c ~sql:"TRUNCATE soak_copy" ~params:[]);
              let written = D.copy raw_c ~sql:"COPY soak_copy (v) FROM STDIN" ~rows in
              let counted =
                D.query raw_c ~sql:"SELECT count(*)::text FROM soak_copy" ~params:[]
                  ~columns:1
              in
              Ok (written, counted))
        in
        match r with
        | Ok (Ok n, Ok [ [| Text counted |] ]) ->
            expect "copy count" (n = List.length rows && int_of_string counted = n)
        | _ -> bump "copy refused")
    | r when r < 88 ->
        bump "cancelled";
        (* cancel at a random moment: before the write, mid-flight, or after
           the answer is already on its way back *)
        let delay = Random.State.float state 0.03 in
        Eio.Fiber.first
          (fun () ->
            ignore
              (use (fun c ->
                   Db.wait_for_server c ~seconds:(Random.State.float state 0.04))))
          (fun () -> Eio.Time.Mono.sleep clock delay)
    | r when r < 97 -> (
        bump "deadline";
        match use (fun c -> Db.wait_for_server c ~seconds:5.) with
        | Error (Sqlkit.Error.Timeout { phase = Query; _ }) -> ()
        | Error _ -> bump "deadline: other error"
        | Ok _ -> expect "five-second sleep beat a half-second deadline" false)
    | _ -> (
        bump "terminated";
        (* kill somebody's backend, possibly idle in the pool, possibly busy *)
        match
          admin_rows
            "SELECT pg_terminate_backend(pid)::text FROM pg_stat_activity WHERE \
             application_name = $1 ORDER BY random() LIMIT 1"
            [ Text tag ]
        with
        | Ok _ -> ()
        | Error e -> expect ("admin: " ^ e.message) false)
  in
  Eio.Fiber.List.iter
    (fun worker ->
      let n = ref 0 in
      while Unix.gettimeofday () < deadline do
        incr n;
        (try step worker !n with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e -> expect (Printf.sprintf "worker raised %s" (Printexc.to_string e)) false);
        Eio.Fiber.yield ()
      done)
    (List.init workers Fun.id);
  (* afterwards: the pool still answers on every slot at once *)
  Eio.Fiber.List.iter
    (fun i ->
      let token = Printf.sprintf "after-%d" i in
      match use (fun c -> Ok (raw c "SELECT $1::text" [ Text token ])) with
      | Ok (Ok [ [| Text t |] ]) -> expect "pool answers after the soak" (t = token)
      | _ -> expect "pool answers after the soak" false)
    (List.init (pool_size * 2) Fun.id);
  (* and the server is not left holding our work *)
  let census () =
    match
      admin_rows
        "SELECT count(*)::text, count(*) FILTER (WHERE state <> 'idle')::text FROM \
         pg_stat_activity WHERE application_name = $1"
        [ Text tag ]
    with
    | Ok [ [| Text total; Text busy |] ] -> (int_of_string total, int_of_string busy)
    | _ -> (-1, -1)
  in
  let rec settle fuel =
    let total, busy = census () in
    if (total <= pool_size && busy = 0) || fuel = 0 then (total, busy)
    else (
      Eio.Time.Mono.sleep clock 0.05;
      settle (fuel - 1))
  in
  let total, busy = settle 100 in
  expect
    (Printf.sprintf "server holds %d backends (%d busy), pool allows %d" total busy
       pool_size)
    (total >= 0 && total <= pool_size && busy = 0);
  let summary =
    Hashtbl.fold (fun k v acc -> Printf.sprintf "%s=%d" k v :: acc) counts []
    |> List.sort compare |> String.concat " "
  in
  Printf.printf "%s %.0fs seed %d: %s\n" driver seconds seed summary;
  match List.sort_uniq compare !wrong with
  | [] -> print_endline "all good"
  | problems ->
      List.iter (Printf.printf "FAIL %s\n") problems;
      exit 1

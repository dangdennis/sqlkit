open Generated

let run ~check ~id conn =
  let uuid =
    Option.get (Uuidm.of_string (Printf.sprintf "1a000000-0000-4000-8000-%012d" id))
  in
  let values =
    [
      Sqlkit.Interval.zero;
      Sqlkit.Interval.make_exn ~months:2147483647 ();
      Sqlkit.Interval.make_exn ~months:(-2147483648) ();
      Sqlkit.Interval.make_exn ~days:2147483647 ();
      Sqlkit.Interval.make_exn ~days:(-2147483648) ();
      Sqlkit.Interval.make_exn ~micros:Int64.max_int ();
      Sqlkit.Interval.make_exn ~micros:Int64.min_int ();
      Sqlkit.Interval.make_exn ~months:2147483647 ~days:(-2147483648)
        ~micros:Int64.max_int ();
      Sqlkit.Interval.make_exn ~months:(-2147483648) ~days:2147483647
        ~micros:Int64.min_int ();
      Sqlkit.Interval.make_exn ~days:(-3) ~seconds:(-6) ~micros:(-700000L) ();
    ]
  in
  Fun.protect
    ~finally:(fun () -> ignore (Db.set_interval_style conn ~style:"postgres"))
    (fun () ->
      List.iter
        (fun style ->
          ignore (Db.set_interval_style_exn conn ~style);
          List.iteri
            (fun i duration ->
              ignore
                (Db.put_booking_exn conn ~id:uuid ~on_date:(2026, 9, 15)
                   ~at_time:Ptime.Span.zero ~duration);
              let row = Db.get_booking_exn conn ~id:uuid in
              check
                (Printf.sprintf "interval %s boundary %d" style i)
                (row.duration = Some duration))
            values)
        [ "postgres"; "postgres_verbose"; "sql_standard"; "iso_8601" ])

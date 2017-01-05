open Lwt.Infix

module Main (Console : Mirage_types_lwt.CONSOLE) (Time : Mirage_types_lwt.TIME) (PClock : Mirage_types_lwt.PCLOCK) (MClock : Mirage_types_lwt.MCLOCK) = struct
  let str_of_time (posix_time, timezone) =
    Format.asprintf "%a" (Ptime.pp_human ?tz_offset_s:timezone ()) posix_time

  let start console _ pclock mclock =
    let rec speak pclock mclock () =
      let current_time = PClock.now_d_ps pclock |> Ptime.v in
      let tz = PClock.current_tz_offset_s pclock in
      let str =
        Printf.sprintf "%Lu nanoseconds have elapsed. At the chime, the time will be %s. \x08 *DING*"
          (Mclock.elapsed_ns mclock) @@ str_of_time (current_time, tz)
      in
      Console.log console str >>= fun () ->
      Time.sleep_ns 1_000_000_000L >>= fun () ->
      speak pclock mclock ()
    in
    speak pclock mclock ()
end

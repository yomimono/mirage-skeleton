open Lwt.Infix

module Main (C: Mirage_types_lwt.CONSOLE) (Time : Mirage_types_lwt.TIME) = struct

  let start c _time =
    let rec loop = function
      | 0 -> Lwt.return_unit
      | n ->
        C.log c (Key_gen.hello ()) >>= fun () ->
        Time.sleep_ns (Duration.of_sec 1) >>= fun () ->
        loop (n-1)
    in
    loop 4

end

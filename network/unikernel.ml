open Lwt.Infix

let red fmt    = Fmt.strf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Fmt.strf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Fmt.strf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Fmt.strf ("\027[36m"^^fmt^^"\027[m")

module Main (C: Mirage_types_lwt.CONSOLE) (S: Mirage_types_lwt.STACKV4) = struct

  let start c s =
    S.listen_tcpv4 s ~port:8080 (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        C.log c (green "new tcp connection from %s %d"
                   (Ipaddr.V4.to_string dst) dst_port) >>= fun () ->
        let b = Cstruct.of_string "🐫\n" in
        S.TCPV4.write flow b >|= Rresult.R.get_ok
      );

    S.listen s

end

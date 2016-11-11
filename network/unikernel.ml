open Lwt.Infix

module Main (S: V1_LWT.STACKV4) = struct

  let src = Logs.Src.create "network" ~doc:"Example hetwork stack user"
  module Log = (val Logs.src_log src: Logs.LOG)

  let start s =
    S.listen_tcpv4 s ~port:8080 (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        Log.info (fun f -> f "new tcp connection from %s %d"
                     (Ipaddr.V4.to_string dst) dst_port);
        S.TCPV4.read flow
        >>= function
        | `Ok b ->
          Log.info (fun f -> f
            "read: %d\n%s" (Cstruct.len b) (Cstruct.to_string b)
          );
          S.TCPV4.close flow
        | `Eof -> Log.info (fun f -> f "read: eof"); Lwt.return_unit
        | `Error _e -> Log.info (fun f -> f "read: error"); Lwt.return_unit
      );

    S.listen s

end

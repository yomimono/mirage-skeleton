open V1_LWT
open Lwt

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

module Main (C: CONSOLE) (FS: KV_RO) (Clock : V1.CLOCK) = struct

  module P = Netif.Make(FS)(OS.Time)
  module E = Ethif.Make(P)
  module A = Arpv4.Make(E)(Clock)(OS.Time)
  module I = Ipv4.Make(E)(A)
  module U = Udp.Make(I)
  module T = Tcp.Flow.Make(I)(OS.Time)(Clock)(Random)
  module D = Dhcp_clientv4.Make(C)(OS.Time)(Random)(U)

  let or_error c name fn t =
    fn t
    >>= function
    | `Error e -> fail (Failure ("Error starting " ^ name))
    | `Ok t -> C.log c "connected!"; return t

  let start c fs _ =
    let pcap_netif_id = P.id_of_desc ~mac:Macaddr.broadcast ~source:fs
        ~timing:None ~read:"packets.pcap" in
    or_error c "Pcap" P.connect pcap_netif_id >>= fun net ->
    or_error c "Ethif" E.connect net
    >>= fun e ->
    or_error c "Arpv4" A.connect e 
    >>= fun a ->
    or_error c "Ipv4" (I.connect ~ip:(Ipaddr.V4.of_string_exn "192.30.252.129") e) a
    >>= fun i ->

    or_error c "UDPv4" U.connect i
    >>= fun udp ->

    let dhcp, offers = D.create c (P.mac net) udp in
    or_error c "TCPv4" T.connect i
    >>= fun tcp ->
    C.log c "All stacks initialized.  Listening...";

    P.listen net (
      E.input
        ~arpv4:(A.input a)
        ~ipv4:(
          I.input
            ~tcp:(
              T.input tcp ~listeners:
                (function
                  | p -> Some (fun flow ->
                      let dst, dst_port = T.get_dest flow in
                      C.log_s c
                        (green "new tcp from %s %d to port %d"
                          (Ipaddr.V4.to_string dst) dst_port p
                        )
                      >>= fun () ->

                      T.read flow
                      >>= function
                      | `Ok b ->
                        C.log_s c
                          (yellow "read: %d\n%s"
                            (Cstruct.len b) (Cstruct.to_string b)
                          )
                        >>= fun () ->
                        T.close flow
                      | `Eof -> C.log_s c (red "read: eof")
                      | `Error e -> C.log_s c (red "read: error"))
                ))
            ~udp:(
              U.input ~listeners:
                (fun ~dst_port ->
                   C.log c (blue "udp packet on port %d" dst_port);
                   D.listen dhcp ~dst_port)
                udp
            )
            ~default:(fun ~proto ~src ~dst _ -> return ())
            i
        )
        ~ipv6:(fun b -> C.log_s c (yellow "ipv6")) e
    )
end

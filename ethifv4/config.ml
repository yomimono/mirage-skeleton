open Mirage

let main =

  let libraries = [
    "mirage-net-pcap"; "pcap-format";
    "tcpip.ethif"; "tcpip.arpv4"; "tcpip.tcp";
    "tcpip.udp"; "tcpip.dhcpv4" ] in
let packages = ["mirage-net-pcap";"pcap-format";"tcpip"] in
  foreign
    ~libraries ~packages
    "Unikernel.Main" (console @-> kv_ro @-> clock @-> job)

let () =
  let disk1 = direct_kv_ro "pcaps" in
  register "ethifv4" [
    main $ default_console $ disk1 $ default_clock
  ]

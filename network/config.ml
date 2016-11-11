open Mirage

let main = foreign "Unikernel.Main" (stackv4 @-> job)

let stack = generic_stackv4 tap0

let () =
  register "network" [
    main $ stack
  ]

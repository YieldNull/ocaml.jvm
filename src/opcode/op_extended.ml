open Core.Std
open Jvalue
open Frame

let op_wide frame = ()

let op_multianewarray frame =
  let index = read_ui16 frame in
  let dimensions = read_byte frame in
  let jclass = Poolrt.get_class frame.conspool index in
  let lens = List.init dimensions ~f:(fun _ -> get_int @@ Stack.pop_exn frame.opstack) in
  let arr = Jarray.create_multiple jclass dimensions lens in
  Stack.push frame.opstack (Reference arr)

let op_ifnull frame = ()
let op_ifnonnull frame = ()
let op_goto_w frame = ()
let op_jsr_w frame = ()

open Core
open Jvalue
open Frame

let op_wide frame = ()

let op_multianewarray frame =
  let index = read_ui16 frame in
  let dimensions = read_byte frame in
  let binary_name = Poolrt.get_class (conspool frame) index in
  let jclass = Classloader.load_class (current_loader frame) binary_name in
  let lens = List.init dimensions ~f:(fun _ -> get_int @@ stack_pop_exn frame) in
  let arr = Jarray.create_multiple jclass dimensions lens in
  stack_push frame (Array arr)

let op_ifnull frame = ()
let op_ifnonnull frame = ()
let op_goto_w frame = ()
let op_jsr_w frame = ()

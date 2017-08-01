open Core
open VMError
open Jvalue
open Frame

let op_iload frame = stack_push frame (Int (get_int @@ localvar_get frame (read_byte frame)))
let op_lload frame = stack_push frame (Long (get_long @@ localvar_get frame (read_byte frame)))
let op_fload frame = stack_push frame (Float (get_float @@ localvar_get frame (read_byte frame)))
let op_dload frame = stack_push frame (Double (get_double @@ localvar_get frame (read_byte frame)))
let op_aload frame = stack_push frame (get_reference @@ localvar_get frame (read_byte frame))

let op_iload_0 frame = stack_push frame (Int (get_int @@ localvar_get frame 0))
let op_iload_1 frame = stack_push frame (Int (get_int @@ localvar_get frame 1))
let op_iload_2 frame = stack_push frame (Int (get_int @@ localvar_get frame 2))
let op_iload_3 frame = stack_push frame (Int (get_int @@ localvar_get frame 3))

let op_lload_0 frame = stack_push frame (Long (get_long @@ localvar_get frame 0))
let op_lload_1 frame = stack_push frame (Long (get_long @@ localvar_get frame 1))
let op_lload_2 frame = stack_push frame (Long (get_long @@ localvar_get frame 2))
let op_lload_3 frame = stack_push frame (Long (get_long @@ localvar_get frame 3))

let op_fload_0 frame = stack_push frame (Float (get_float @@ localvar_get frame 0))
let op_fload_1 frame = stack_push frame (Float (get_float @@ localvar_get frame 1))
let op_fload_2 frame = stack_push frame (Float (get_float @@ localvar_get frame 2))
let op_fload_3 frame = stack_push frame (Float (get_float @@ localvar_get frame 3))

let op_dload_0 frame = stack_push frame (Double (get_double @@ localvar_get frame 0))
let op_dload_1 frame = stack_push frame (Double (get_double @@ localvar_get frame 1))
let op_dload_2 frame = stack_push frame (Double (get_double @@ localvar_get frame 2))
let op_dload_3 frame = stack_push frame (Double (get_double @@ localvar_get frame 3))

let op_aload_0 frame = stack_push frame (get_reference @@ localvar_get frame 0)
let op_aload_1 frame = stack_push frame (get_reference @@ localvar_get frame 1)
let op_aload_2 frame = stack_push frame (get_reference @@ localvar_get frame 2)
let op_aload_3 frame = stack_push frame (get_reference @@ localvar_get frame 3)

let op_iaload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_int @@ Jarray.load arr index in
  stack_push frame (Int value)

let op_laload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_long @@ Jarray.load arr index in
  stack_push frame (Long value)

let op_faload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_float @@ Jarray.load arr index in
  stack_push frame (Float value)

let op_daload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_double @@ Jarray.load arr index in
  stack_push frame (Double value)

let op_aaload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_reference @@ Jarray.load arr index in
  stack_push frame value

let op_baload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  match Jarray.load arr index with
  | Byte b -> stack_push frame (Int (Int32.of_int_exn b))
  | Boolean b -> stack_push frame (Int (Int32.of_int_exn b))
  | _ -> raise VirtualMachineError

let op_caload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_char @@ Jarray.load arr index in
  stack_push frame (Int (Int32.of_int_exn value))

let op_saload frame =
  let index = get_int @@ stack_pop_exn frame in
  let arr = get_array @@ stack_pop_exn frame in
  let value = get_short @@ Jarray.load arr index in
  stack_push frame (Int (Int32.of_int_exn value))

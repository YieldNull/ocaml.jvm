open Core.Std
open Jvalue
open Frame

(* TODO The target address must be
   that of an opcode of an instruction within the method
   that contains this goto instruction.*)
let op_goto frame =
  let offset = read_ui16 frame in
  set_pc_offset frame (offset - 3)

let op_jsr frame = ()
let op_ret frame = ()
let op_tableswitch frame = ()
let op_lookupswitch frame = ()

let op_ireturn frame = ()
let op_lreturn frame = ()
let op_freturn frame = ()
let op_dreturn frame = ()
let op_areturn frame = ()
let op_return frame = ()

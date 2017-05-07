open Core.Std
open Jvalue
open Frame

let op_istore frame =
  frame.localvars.(read_byte frame) <- Int (get_int @@ Stack.pop_exn frame.opstack)

let op_lstore frame =
  frame.localvars.(read_byte frame) <- Long (get_long @@ Stack.pop_exn frame.opstack)

let op_fstore frame =
  frame.localvars.(read_byte frame) <- Float (get_float @@ Stack.pop_exn frame.opstack)

let op_dstore frame =
  frame.localvars.(read_byte frame) <- Double (get_double @@ Stack.pop_exn frame.opstack)

let op_astore frame =
  frame.localvars.(read_byte frame) <- Reference (get_reference @@ Stack.pop_exn frame.opstack)

let op_istore_0 frame =
  frame.localvars.(0) <- Int (get_int @@ Stack.pop_exn frame.opstack)

let op_istore_1 frame =
  frame.localvars.(1) <- Int (get_int @@ Stack.pop_exn frame.opstack)

let op_istore_2 frame =
  frame.localvars.(2) <- Int (get_int @@ Stack.pop_exn frame.opstack)

let op_istore_3 frame =
  frame.localvars.(3) <- Int (get_int @@ Stack.pop_exn frame.opstack)

let op_lstore_0 frame =
  frame.localvars.(0) <- Long (get_long @@ Stack.pop_exn frame.opstack)

let op_lstore_1 frame =
  frame.localvars.(1) <- Long (get_long @@ Stack.pop_exn frame.opstack)

let op_lstore_2 frame =
  frame.localvars.(2) <- Long (get_long @@ Stack.pop_exn frame.opstack)

let op_lstore_3 frame =
  frame.localvars.(3) <- Long (get_long @@ Stack.pop_exn frame.opstack)

let op_fstore_0 frame =
  frame.localvars.(0) <- Float (get_float @@ Stack.pop_exn frame.opstack)

let op_fstore_1 frame =
  frame.localvars.(1) <- Float (get_float @@ Stack.pop_exn frame.opstack)

let op_fstore_2 frame =
  frame.localvars.(2) <- Float (get_float @@ Stack.pop_exn frame.opstack)

let op_fstore_3 frame =
  frame.localvars.(3) <- Float (get_float @@ Stack.pop_exn frame.opstack)

let op_dstore_0 frame =
  frame.localvars.(0) <- Double (get_double @@ Stack.pop_exn frame.opstack)

let op_dstore_1 frame =
  frame.localvars.(1) <- Double (get_double @@ Stack.pop_exn frame.opstack)

let op_dstore_2 frame =
  frame.localvars.(2) <- Double (get_double @@ Stack.pop_exn frame.opstack)

let op_dstore_3 frame =
  frame.localvars.(3) <- Double (get_double @@ Stack.pop_exn frame.opstack)

let op_astore_0 frame =
  frame.localvars.(0) <- Reference (get_reference @@ Stack.pop_exn frame.opstack)

let op_astore_1 frame =
  frame.localvars.(1) <- Reference (get_reference @@ Stack.pop_exn frame.opstack)

let op_astore_2 frame =
  frame.localvars.(2) <- Reference (get_reference @@ Stack.pop_exn frame.opstack)

let op_astore_3 frame =
  frame.localvars.(3) <- Reference (get_reference @@ Stack.pop_exn frame.opstack)

let op_iastore frame =
  let value = Stack.pop_exn frame.opstack in
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  Jarray.store arr index value

let op_lastore frame = op_iastore frame
let op_fastore frame = op_iastore frame
let op_dastore frame = op_iastore frame
let op_aastore frame = op_iastore frame
let op_bastore frame = op_iastore frame
let op_castore frame = op_iastore frame
let op_sastore frame = op_iastore frame

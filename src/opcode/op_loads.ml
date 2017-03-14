open Core.Std
open Jvalue
open Frame

let op_iload frame = Stack.push frame.opstack (Int (get_int frame.localvars.(read_byte frame)))
let op_lload frame = Stack.push frame.opstack (Long (get_long frame.localvars.(read_byte frame)))
let op_fload frame = Stack.push frame.opstack (Float (get_float frame.localvars.(read_byte frame)))
let op_dload frame = Stack.push frame.opstack (Double (get_double frame.localvars.(read_byte frame)))
let op_aload frame = Stack.push frame.opstack (Reference (get_reference frame.localvars.(read_byte frame)))

let op_iload_0 frame = Stack.push frame.opstack (Int (get_int frame.localvars.(0)))
let op_iload_1 frame = Stack.push frame.opstack (Int (get_int frame.localvars.(1)))
let op_iload_2 frame = Stack.push frame.opstack (Int (get_int frame.localvars.(2)))
let op_iload_3 frame = Stack.push frame.opstack (Int (get_int frame.localvars.(3)))

let op_lload_0 frame = Stack.push frame.opstack (Long (get_long frame.localvars.(0)))
let op_lload_1 frame = Stack.push frame.opstack (Long (get_long frame.localvars.(1)))
let op_lload_2 frame = Stack.push frame.opstack (Long (get_long frame.localvars.(2)))
let op_lload_3 frame = Stack.push frame.opstack (Long (get_long frame.localvars.(3)))

let op_fload_0 frame = Stack.push frame.opstack (Float (get_float frame.localvars.(0)))
let op_fload_1 frame = Stack.push frame.opstack (Float (get_float frame.localvars.(1)))
let op_fload_2 frame = Stack.push frame.opstack (Float (get_float frame.localvars.(2)))
let op_fload_3 frame = Stack.push frame.opstack (Float (get_float frame.localvars.(3)))

let op_dload_0 frame = Stack.push frame.opstack (Double (get_double frame.localvars.(0)))
let op_dload_1 frame = Stack.push frame.opstack (Double (get_double frame.localvars.(1)))
let op_dload_2 frame = Stack.push frame.opstack (Double (get_double frame.localvars.(2)))
let op_dload_3 frame = Stack.push frame.opstack (Double (get_double frame.localvars.(3)))

let op_aload_0 frame = Stack.push frame.opstack (Reference (get_reference frame.localvars.(0)))
let op_aload_1 frame = Stack.push frame.opstack (Reference (get_reference frame.localvars.(1)))
let op_aload_2 frame = Stack.push frame.opstack (Reference (get_reference frame.localvars.(2)))
let op_aload_3 frame = Stack.push frame.opstack (Reference (get_reference frame.localvars.(3)))

let op_iaload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_int @@ Jobject.load arr index in
  Stack.push frame.opstack (Int value)

let op_laload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_long @@ Jobject.load arr index in
  Stack.push frame.opstack (Long value)

let op_faload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_float @@ Jobject.load arr index in
  Stack.push frame.opstack (Float value)

let op_daload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_double @@ Jobject.load arr index in
  Stack.push frame.opstack (Double value)

let op_aaload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_reference @@ Jobject.load arr index in
  Stack.push frame.opstack (Reference value)

let op_baload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_byte @@ Jobject.load arr index in
  Stack.push frame.opstack (Byte value)

let op_caload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_char @@ Jobject.load arr index in
  Stack.push frame.opstack (Char value)

let op_saload frame =
  let index = get_int @@ Stack.pop_exn frame.opstack in
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let value = get_short @@ Jobject.load arr index in
  Stack.push frame.opstack (Short value)

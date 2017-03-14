open Core.Std
open Jvalue
open Frame
open VMError

let op_iadd frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let sum = Caml.Int32.add value1 value2 in
  Stack.push frame.opstack (Int sum)

let op_ladd frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let sum = Caml.Int64.add value1 value2 in
  Stack.push frame.opstack (Long sum)

let op_fadd frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  let sum = Float32.add value1 value2 in
  Stack.push frame.opstack (Float sum)

let op_dadd frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  let sum = value1 +. value2 in
  Stack.push frame.opstack (Double sum)

let op_isub frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let sub = Caml.Int32.sub value1 value2 in
  Stack.push frame.opstack (Int sub)

let op_lsub frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let sub = Caml.Int64.sub value1 value2 in
  Stack.push frame.opstack (Long sub)

let op_fsub frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  let sub = Float32.sub value1 value2 in
  Stack.push frame.opstack (Float sub)

let op_dsub frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  let sub = value1 -. value2 in
  Stack.push frame.opstack (Double sub)

let op_imul frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let sub = Caml.Int32.mul value1 value2 in
  Stack.push frame.opstack (Int sub)

let op_lmul frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let sub = Caml.Int64.mul value1 value2 in
  Stack.push frame.opstack (Long sub)

let op_fmul frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  let sub = Float32.mul value1 value2 in
  Stack.push frame.opstack (Float sub)

let op_dmul frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  let sub = value1 *. value2 in
  Stack.push frame.opstack (Double sub)

let op_idiv frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  if Int32.equal value2 0l then raise ArithmeticException;
  let sub = Caml.Int32.div value1 value2 in
  Stack.push frame.opstack (Int sub)

let op_ldiv frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  if Int64.equal value2 0L then raise ArithmeticException;
  let sub = Caml.Int64.div value1 value2 in
  Stack.push frame.opstack (Long sub)

let op_fdiv frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  if Float32.equal value2 Float32.zero then raise ArithmeticException;
  let sub = Float32.div value1 value2 in
  Stack.push frame.opstack (Float sub)

let op_ddiv frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  if value2 = 0. then raise ArithmeticException;
  let sub = value1 /. value2 in
  Stack.push frame.opstack (Double sub)

let op_irem frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let rem = Caml.Int32.rem value1 value2 in
  Stack.push frame.opstack (Int rem)

let op_lrem frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let rem = Caml.Int64.rem value1 value2 in
  Stack.push frame.opstack (Long rem)

let op_frem frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  let rem = Float32.rem value1 value2 in
  Stack.push frame.opstack (Float rem)

let op_drem frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  let rem = Float.mod_float value1 value2 in
  Stack.push frame.opstack (Double rem)

let op_ineg frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  Stack.push frame.opstack (Int (Int32.neg value))

let op_lneg frame =
  let value = get_long @@ Stack.pop_exn frame.opstack in
  Stack.push frame.opstack (Long (Int64.neg value))

let op_fneg frame =
  let value = get_float @@ Stack.pop_exn frame.opstack in
  Stack.push frame.opstack (Float (Float32.neg value))

let op_dneg frame =
  let value = get_double @@ Stack.pop_exn frame.opstack in
  Stack.push frame.opstack (Double (-. value))

let op_ishl frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let shl = Int32.shift_left value1 (Caml.Int32.to_int value2) in
  Stack.push frame.opstack (Int shl)

let op_lshl frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let shl = Int64.shift_left value1 (Caml.Int64.to_int value2) in
  Stack.push frame.opstack (Long shl)

let op_ishr frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let shl = Int32.shift_right  value1 (Caml.Int32.to_int value2) in
  Stack.push frame.opstack (Int shl)

let op_lshr frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let shl = Int64.shift_right value1 (Caml.Int64.to_int value2) in
  Stack.push frame.opstack (Long shl)

let op_iushr frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let shl = Int32.shift_right_logical value1 (Caml.Int32.to_int value2) in
  Stack.push frame.opstack (Int shl)

let op_lushr frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let shl = Int64.shift_right_logical value1 (Caml.Int64.to_int value2) in
  Stack.push frame.opstack (Long shl)

let op_iand frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int32.logand value1 value2 in
  Stack.push frame.opstack (Int result)

let op_land frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.logand value1 value2 in
  Stack.push frame.opstack (Long result)

let op_ior frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int32.logor value1 value2 in
  Stack.push frame.opstack (Int result)

let op_lor frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.logor value1 value2 in
  Stack.push frame.opstack (Long result)

let op_ixor frame =
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int32.logxor value1 value2 in
  Stack.push frame.opstack (Int result)

let op_lxor frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.logxor value1 value2 in
  Stack.push frame.opstack (Long result)

let op_iinc frame =
  let index = read_byte frame in
  let value = get_int frame.localvars.(index) in
  let incre = Caml.Int32.of_int (read_signed_byte frame) in
  frame.localvars.(index) <- Int (Caml.Int32.add value incre)

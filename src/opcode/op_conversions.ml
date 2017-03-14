open Core.Std
open Jvalue
open Frame

let op_i2l frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.of_int32 value in
  Stack.push frame.opstack (Long result)

let op_i2f frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  let result = Float32.of_int32 value in
  Stack.push frame.opstack (Float result)

let op_i2d frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int32.to_float value in
  Stack.push frame.opstack (Double result)

let op_l2i frame =
  let value = get_long @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.to_int32 value in
  Stack.push frame.opstack (Int result)

let op_l2f frame =
  let value = get_long @@ Stack.pop_exn frame.opstack in
  let result = Float32.of_int64 value in
  Stack.push frame.opstack (Float result)

let op_l2d frame =
  let value = get_long @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.to_float value in
  Stack.push frame.opstack (Double result)

let op_f2i frame =
  let value = get_float @@ Stack.pop_exn frame.opstack in
  let result = Float32.to_int32 value in
  Stack.push frame.opstack (Int result)

let op_f2l frame =
  let value = get_float @@ Stack.pop_exn frame.opstack in
  let result = Float32.to_int64 value in
  Stack.push frame.opstack (Long result)

let op_f2d frame =
  let value = get_float @@ Stack.pop_exn frame.opstack in
  let result = Float32.to_float64 value in
  Stack.push frame.opstack (Double result)

let op_d2i frame =
  let value = get_double @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int32.of_float value in
  Stack.push frame.opstack (Int result)

let op_d2l frame =
  let value = get_double @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int64.of_float value in
  Stack.push frame.opstack (Long result)

let op_d2f frame =
  let value = get_double @@ Stack.pop_exn frame.opstack in
  let result = Float32.of_float64 value in
  Stack.push frame.opstack (Float result)

let op_i2b frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  let byte = Caml.Int32.logand 0xffl value in
  let result = if Caml.Int32.equal (Caml.Int32.logand 0x80l byte) 0l
    then byte else Caml.Int32.add 0xffffff00l byte
  in
  Stack.push frame.opstack (Int result)

let op_i2c frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  let result = Caml.Int32.logand 0xffl value in
  Stack.push frame.opstack (Int result)

let op_i2s frame =
  let value = get_int @@ Stack.pop_exn frame.opstack in
  let byte = Caml.Int32.logand 0xffffl value in
  let result = if Caml.Int32.equal (Caml.Int32.logand 0x8000l byte) 0l
    then byte else Caml.Int32.add 0xffff0000l byte
  in
  Stack.push frame.opstack (Int result)

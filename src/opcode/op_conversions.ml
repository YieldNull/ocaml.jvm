open Jvalue
open Frame

let op_i2l frame =
  let value = get_int @@ stack_pop_exn frame in
  let result = Int64.of_int32 value in
  stack_push frame (Long result)

let op_i2f frame =
  let value = get_int @@ stack_pop_exn frame in
  let result = Float32.of_int32 value in
  stack_push frame (Float result)

let op_i2d frame =
  let value = get_int @@ stack_pop_exn frame in
  let result = Int32.to_float value in
  stack_push frame (Double result)

let op_l2i frame =
  let value = get_long @@ stack_pop_exn frame in
  let result = Int64.to_int32 value in
  stack_push frame (Int result)

let op_l2f frame =
  let value = get_long @@ stack_pop_exn frame in
  let result = Float32.of_int64 value in
  stack_push frame (Float result)

let op_l2d frame =
  let value = get_long @@ stack_pop_exn frame in
  let result = Int64.to_float value in
  stack_push frame (Double result)

let op_f2i frame =
  let value = get_float @@ stack_pop_exn frame in
  let result = Float32.to_int32 value in
  stack_push frame (Int result)

let op_f2l frame =
  let value = get_float @@ stack_pop_exn frame in
  let result = Float32.to_int64 value in
  stack_push frame (Long result)

let op_f2d frame =
  let value = get_float @@ stack_pop_exn frame in
  let result = Float32.to_float64 value in
  stack_push frame (Double result)

let op_d2i frame =
  let value = get_double @@ stack_pop_exn frame in
  let result = Int32.of_float value in
  stack_push frame (Int result)

let op_d2l frame =
  let value = get_double @@ stack_pop_exn frame in
  let result = Int64.of_float value in
  stack_push frame (Long result)

let op_d2f frame =
  let value = get_double @@ stack_pop_exn frame in
  let result = Float32.of_float64 value in
  stack_push frame (Float result)

let op_i2b frame =
  let value = get_int @@ stack_pop_exn frame in
  let byte = Int32.logand 0xffl value in
  let result = if Int32.equal (Int32.logand 0x80l byte) 0l
    then byte else Int32.add 0xffffff00l byte
  in
  stack_push frame (Int result)

let op_i2c frame =
  let value = get_int @@ stack_pop_exn frame in
  let result = Int32.logand 0xffl value in
  stack_push frame (Int result)

let op_i2s frame =
  let value = get_int @@ stack_pop_exn frame in
  let byte = Int32.logand 0xffffl value in
  let result = if Int32.equal (Int32.logand 0x8000l byte) 0l
    then byte else Int32.add 0xffff0000l byte
  in
  stack_push frame (Int result)

open Jvalue
open Frame
open Prim

let op_i2l frame =
  let value = i2l @@ get_int @@ stack_pop_exn frame in
  stack_push frame (Long value)

let op_i2f frame =
  let value = i2f @@ get_int @@ stack_pop_exn frame in
  stack_push frame (Float value)

let op_i2d frame =
  let value = i2d @@ get_int @@ stack_pop_exn frame in
  stack_push frame (Double value)

let op_l2i frame =
  let value = l2i @@ get_long @@ stack_pop_exn frame in
  stack_push frame (Int value)

let op_l2f frame =
  let value = l2f @@ get_long @@ stack_pop_exn frame in
  stack_push frame (Float value)

let op_l2d frame =
  let value = l2d @@ get_long @@ stack_pop_exn frame in
  stack_push frame (Double value)

let op_f2i frame =
  let value = f2i @@ get_float @@ stack_pop_exn frame in
  stack_push frame (Int value)

let op_f2l frame =
  let value = f2l @@ get_float @@ stack_pop_exn frame in
  stack_push frame (Long value)

let op_f2d frame =
  let value = f2d @@ get_float @@ stack_pop_exn frame in
  stack_push frame (Double value)

let op_d2i frame =
  let value = d2i @@ get_double @@ stack_pop_exn frame in
  stack_push frame (Int value)

let op_d2l frame =
  let value = d2l @@ get_double @@ stack_pop_exn frame in
  stack_push frame (Long value)

let op_d2f frame =
  let value = d2f @@ get_double @@ stack_pop_exn frame in
  stack_push frame (Float value)

let op_i2b frame =
  let value = i2b @@ get_int @@ stack_pop_exn frame in
  stack_push frame (Int value)

let op_i2c frame =
  let value = i2c @@ get_int @@ stack_pop_exn frame in
  stack_push frame (Int value)

let op_i2s frame =
  let value = i2s @@ get_int @@ stack_pop_exn frame in
  stack_push frame (Int value)

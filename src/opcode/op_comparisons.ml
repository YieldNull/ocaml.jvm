open Core
open Jvalue
open Frame

let op_lcmp frame =
  let value2 = get_long @@ stack_pop_exn frame in
  let value1 = get_long @@ stack_pop_exn frame in
  let result = Int64.compare value1 value2 in
  let cmp = if result > 0 then 1l else if result < 0 then -1l else 0l in
  stack_push frame (Int cmp)

let op_fcmpl frame =
  let value2 = get_float @@ stack_pop_exn frame in
  let value1 = get_float @@ stack_pop_exn frame in
  if Float32.is_nan value1 || Float32.is_nan value2 then
    stack_push frame (Int (-1l))
  else
    stack_push frame (Int (Int32.of_int_exn (Float32.compare value1 value2)))

let op_fcmpg frame =
  let value2 = get_float @@ stack_pop_exn frame in
  let value1 = get_float @@ stack_pop_exn frame in
  if Float32.is_nan value1 || Float32.is_nan value2 then
    stack_push frame (Int 1l)
  else
    stack_push frame (Int (Int32.of_int_exn (Float32.compare value1 value2)))

let op_dcmpl frame =
  let value2 = get_double @@ stack_pop_exn frame in
  let value1 = get_double @@ stack_pop_exn frame in
  if Float.is_nan value1 || Float.is_nan value2 then
    stack_push frame (Int (-1l))
  else
    let result = compare value1 value2 in
    let cmp = if result > 0 then 1l else if result < 0 then -1l else 0l in
    stack_push frame (Int cmp)

let op_dcmpg frame =
  let value2 = get_double @@ stack_pop_exn frame in
  let value1 = get_double @@ stack_pop_exn frame in
  if Float.is_nan value1 || Float.is_nan value2 then
    stack_push frame (Int 1l)
  else
    let result = compare value1 value2 in
    let cmp = if result > 0 then 1l else if result < 0 then -1l else 0l in
    stack_push frame (Int cmp)

let if_cmp frame ~f =
  let offset = read_ui16 frame in
  let value = get_int @@ stack_pop_exn frame in
  if f value 0l then
    set_pc_offset frame (offset - 3)

let if_icmp frame ~f =
  let offset = read_ui16 frame in
  let value2 = get_int @@ stack_pop_exn frame in
  let value1 = get_int @@ stack_pop_exn frame in
  if f value1 value2 then
    set_pc_offset frame (offset - 3)

let op_ifeq frame = if_cmp frame ~f:Int32.(=)
let op_ifne frame = if_cmp frame ~f:Int32.(<>)
let op_iflt frame = if_cmp frame ~f:Int32.(<)
let op_ifge frame = if_cmp frame ~f:Int32.(>=)
let op_ifgt frame = if_cmp frame ~f:Int32.(>)
let op_ifle frame = if_cmp frame ~f:Int32.(<=)

let op_if_icmpeq frame = if_icmp frame ~f:Int32.(=)
let op_if_icmpne frame = if_icmp frame ~f:Int32.(<>)
let op_if_icmplt frame = if_icmp frame ~f:Int32.(<)
let op_if_icmpge frame = if_icmp frame ~f:Int32.(>=)
let op_if_icmpgt frame = if_icmp frame ~f:Int32.(>)
let op_if_icmple frame = if_icmp frame ~f:Int32.(<=)

let aequal frame =
  let value2 = get_reference @@ stack_pop_exn frame in
  let value1 = get_reference @@ stack_pop_exn frame in
  match (value1, value2) with
  | (Null, Null) -> true
  | (Object obj, Object obj2) -> Jobject.equal obj obj2
  | (Array arr, Array arr2) -> Jarray.equal arr arr2
  | _ -> false

let op_if_acmpeq frame =
  let offset = read_ui16 frame in
  if aequal frame then
    set_pc_offset frame (offset - 3)

let op_if_acmpne frame =
  let offset = read_ui16 frame in
  if not @@ aequal frame then
    set_pc_offset frame (offset - 3)

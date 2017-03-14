open Core.Std
open Jvalue
open Frame

let op_lcmp frame =
  let value2 = get_long @@ Stack.pop_exn frame.opstack in
  let value1 = get_long @@ Stack.pop_exn frame.opstack in
  let result = Int64.compare value1 value2 in
  let cmp = if result > 0 then 1l else if result < 0 then -1l else 0l in
  Stack.push frame.opstack (Int cmp)

let op_fcmpl frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  if Float32.is_nan value1 || Float32.is_nan value2 then
    Stack.push frame.opstack (Int (-1l))
  else
    Stack.push frame.opstack (Int (Float32.compare value1 value2))

let op_fcmpg frame =
  let value2 = get_float @@ Stack.pop_exn frame.opstack in
  let value1 = get_float @@ Stack.pop_exn frame.opstack in
  if Float32.is_nan value1 || Float32.is_nan value2 then
    Stack.push frame.opstack (Int 1l)
  else
    Stack.push frame.opstack (Int (Float32.compare value1 value2))

let op_dcmpl frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  if Float.is_nan value1 || Float.is_nan value2 then
    Stack.push frame.opstack (Int (-1l))
  else
    let result = compare value1 value2 in
    let cmp = if result > 0 then 1l else if result < 0 then -1l else 0l in
    Stack.push frame.opstack (Int cmp)

let op_dcmpg frame =
  let value2 = get_double @@ Stack.pop_exn frame.opstack in
  let value1 = get_double @@ Stack.pop_exn frame.opstack in
  if Float.is_nan value1 || Float.is_nan value2 then
    Stack.push frame.opstack (Int 1l)
  else
    let result = compare value1 value2 in
    let cmp = if result > 0 then 1l else if result < 0 then -1l else 0l in
    Stack.push frame.opstack (Int cmp)

(* TODO ensure The target address must be
   that of an opcode of an instruction
   within the method that contains this if<cond> instruction *)
let if_cmp frame ~f =
  let offset = read_ui16 frame in
  let value = get_int @@ Stack.pop_exn frame.opstack in
  if f value 0l then
    set_pc_offset frame (offset - 3)

let if_icmp frame ~f =
  let offset = read_ui16 frame in
  let value2 = get_int @@ Stack.pop_exn frame.opstack in
  let value1 = get_int @@ Stack.pop_exn frame.opstack in
  if f value1 value2 then
    set_pc_offset frame (offset - 3)

let if_acmp frame ~f =
  let offset = read_ui16 frame in
  let value2 = get_reference @@ Stack.pop_exn frame.opstack in
  let value1 = get_reference @@ Stack.pop_exn frame.opstack in
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

let op_if_acmpeq frame = if_acmp frame ~f:(=) (* TODO test *)
let op_if_acmpne frame = if_acmp frame ~f:(<>)

open Core.Std
open Jvalue
open Frame

let op_getstatic frame = ()
let op_putstatic frame = ()
let op_getfield frame = ()
let op_putfield frame = ()

let op_invokevirtual frame = ()
let op_invokespecial frame = ()
let op_invokestatic frame = ()
let op_invokeinterface frame = ()
let op_invokedynamic frame = ()

let op_new frame =
  let conspool = get_conspool frame in
  let index = read_ui16 frame in
  let jclass = Poolrt.get_class conspool index in
  let obj = Jobject.create_obj jclass in
  Stack.push frame.opstack (Reference obj)

let op_newarray frame =
  let count = get_int @@ Stack.pop_exn frame.opstack in
  let arr = Jobject.create_arr count (read_byte frame) in
  Stack.push frame.opstack (Reference arr)

let op_anewarray frame =
  let index = read_ui16 frame in
  let count = get_int @@ Stack.pop_exn frame.opstack in
  let conspool = get_conspool frame in
  let jclass = Poolrt.get_class conspool index in
  let arr = Jobject.create_ref_arr jclass count in
  Stack.push frame.opstack (Reference arr)

let op_arraylength frame =
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let len = Jobject.get_array_length arr in
  Stack.push frame.opstack (Int len)

let op_athrow frame = ()
let op_checkcast frame = ()
let op_instanceof frame = ()
let op_monitorenter frame = ()
let op_monitorexit frame = ()

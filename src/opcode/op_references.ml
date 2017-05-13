open Core.Std
open Jvalue
open Frame
open VMError

(* TODO initialize class *ALL* *)
let op_getstatic frame =
  let index = read_ui16 frame in
  let jfield = Poolrt.get_field frame.conspool index in
  let value = Jfield.get_static_value jfield in
  Stack.push frame.opstack value

(* TODO the value must be of a type that is assignment compatible (JLS §5.2)
   with the field descriptor type*)
let op_putstatic frame =
  let index = read_ui16 frame in
  let jfield = Poolrt.get_field frame.conspool index in
  let value = Stack.pop_exn frame.opstack in begin
    match Descriptor.type_of_field jfield.Jfield.mid.MemberID.descriptor with
      (Descriptor.Boolean
      | Descriptor.Byte
      | Descriptor.Char
      | Descriptor.Short
      | Descriptor.Int
      ) -> must_be_int value
    | Descriptor.Float -> must_be_float value
    | Descriptor.Long -> must_be_long value
    | Descriptor.Double -> must_be_double value
    | Descriptor.Class _ -> must_be_reference value
  end;
  if Accflag.FlagField.is_set jfield.Jfield.access_flags Accflag.FlagField.Final then
    let jclass = jfield.Jfield.jclass in
    if jclass <> Frame.current_class frame ||
       frame.jmethod.Jmethod.mid.MemberID.name <> "<clinit>" then
      raise IllegalAccessError

let get_obj_exn objref =
  match objref with
  | Jobject.Obj obj -> obj
  | Jobject.Null -> raise NullPointerException
  | _ -> raise VirtualMachineError

(* TODO field is protected *)
let op_getfield frame =
  let jobject = get_reference @@ Stack.pop_exn frame.opstack in
  let index = read_ui16 frame in
  let jfield = Poolrt.get_field frame.conspool index in
  let value = Jobject.get_field_value_exn (get_obj_exn jobject) jfield.Jfield.mid in
  Stack.push frame.opstack value

let op_putfield frame =
  let index = read_ui16 frame in
  let jfield = Poolrt.get_field frame.conspool index in
  let value = Stack.pop_exn frame.opstack in
  let jobject = get_reference @@ Stack.pop_exn frame.opstack in
  begin
    match Descriptor.type_of_field jfield.Jfield.mid.MemberID.descriptor with
      (Descriptor.Boolean
      | Descriptor.Byte
      | Descriptor.Char
      | Descriptor.Short
      | Descriptor.Int
      ) -> must_be_int value
    | Descriptor.Float -> must_be_float value
    | Descriptor.Long -> must_be_long value
    | Descriptor.Double -> must_be_double value
    | Descriptor.Class _ -> must_be_reference value
  end;
  begin
    if Accflag.FlagField.is_set jfield.Jfield.access_flags Accflag.FlagField.Final then
      let jclass = jfield.Jfield.jclass in
      if jclass <> Frame.current_class frame ||
         frame.jmethod.Jmethod.mid.MemberID.name <> "<init>" then
        raise IllegalAccessError
  end;
  Jobject.set_field_value_exn (get_obj_exn jobject) jfield.Jfield.mid value

let op_invokevirtual frame = ()
let op_invokespecial frame = ()
let op_invokestatic frame = ()
let op_invokeinterface frame = ()
let op_invokedynamic frame = ()

let op_new frame =
  let index = read_ui16 frame in
  let jclass = Poolrt.get_class frame.conspool index in
  let obj = Jobject.create jclass in
  Stack.push frame.opstack (Reference (Jobject.Obj obj))

let op_newarray frame =
  let count = get_int @@ Stack.pop_exn frame.opstack in
  let arr = Jarray.create_primitive count (read_byte frame) in
  Stack.push frame.opstack (Reference arr)

let op_anewarray frame =
  let index = read_ui16 frame in
  let count = get_int @@ Stack.pop_exn frame.opstack in
  let jclass = Poolrt.get_class frame.conspool index in
  let arr = Jarray.create_reference jclass count in
  Stack.push frame.opstack (Reference arr)

let op_arraylength frame =
  let arr = get_reference @@ Stack.pop_exn frame.opstack in
  let len = Jarray.length arr in
  Stack.push frame.opstack (Int len)

let op_athrow frame = ()
let op_checkcast frame = ()
let op_instanceof frame = ()

let op_monitorenter frame = ()
let op_monitorexit frame = ()

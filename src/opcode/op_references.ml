open Core.Std
open Jvalue
open Frame
open VMError
open Accflag

(* TODO initialize class *)
let get_field frame index =
  let pool = conspool frame in
  match Poolrt.get pool index with
  | Poolrt.Fieldref x -> x
  | Poolrt.UnresolvedFieldref (cls,mid) ->
    let jfield = Classloader.resolve_field (current_class frame) cls mid in
    Poolrt.set pool index (Poolrt.Fieldref jfield);
    jfield
  | _ -> raise VirtualMachineError

let get_special_method_exn frame index =
  let pool = conspool frame in
  match Poolrt.get pool index with
  | Poolrt.Methodref x -> x
  | Poolrt.UnresolvedMethodref(cls,mid) ->
    let jmethod = Classloader.resolve_method_of_class (current_class frame) cls mid in
    Poolrt.set pool index (Poolrt.Methodref jmethod);
    jmethod
  | _ -> raise VirtualMachineError

let get_class_method frame index =
  let pool = conspool frame in
  match Poolrt.get pool index with
  | Poolrt.Methodref x -> Some x
  | Poolrt.UnresolvedMethodref(cls,mid) ->
    if mid.MemberID.name = "<init>" || mid.MemberID.name = "<cinit>" then
      raise (NoSuchMethodError "");
    let jmethod = Classloader.resolve_method_of_class (current_class frame) cls mid in
    Poolrt.set pool index (Poolrt.Methodref jmethod);
    Some jmethod
  | _ -> None

let get_class_method_exn frame index =
  match get_class_method frame index with
  | Some m -> m
  | _ -> raise VirtualMachineError

let get_interface_method frame index =
  let pool = conspool frame in
  match Poolrt.get pool index with
  | Poolrt.InterfaceMethodref x -> Some x
  | Poolrt.UnresolvedInterfaceMethodref(cls,mid) ->
    if mid.MemberID.name = "<init>" || mid.MemberID.name = "<cinit>" then
      raise (NoSuchMethodError "");
    let jmethod = Classloader.resolve_method_of_interface (current_class frame) cls mid in
    Poolrt.set pool index (Poolrt.InterfaceMethodref jmethod);
    Some jmethod
  | _ -> raise VirtualMachineError

let get_interface_method_exn frame index =
  match get_interface_method frame index with
  | Some m -> m
  | _ -> raise VirtualMachineError

let get_static_method_exn frame index =
  let check_acc jmethod =
    if not (Jmethod.is_static jmethod && (not (Jmethod.is_abstract jmethod))) then
      raise IncompatibleClassChangeError
  in
  match get_class_method frame index with
  | Some m -> check_acc m; m
  | _ -> match get_interface_method frame index with
    | Some m -> check_acc m; m
    | _ -> raise VirtualMachineError

let validate_protected_field frame objcls jfield =
  if Jfield.is_protected jfield then
    let current = current_class frame in
    if Jclass.is_member_of_supper current jfield.Jfield.mid
    && not (Jclass.package_rt_equal current jfield.Jfield.jclass)
    then
      if not (Jclass.is_same_class objcls current ||
              Jclass.is_subclass ~sub:objcls ~super:current)
      then
        raise IllegalAccessError

let validate_protected_method frame objcls jmethod =
  if Jmethod.is_protected jmethod then
    let current = current_class frame in
    if Jclass.is_member_of_supper current (Jmethod.memid jmethod)
    && not (Jclass.package_rt_equal current (Jmethod.jclass jmethod))
    then
      if not (Jclass.is_same_class objcls current ||
              Jclass.is_subclass ~sub:objcls ~super:current)
      then
        raise IllegalAccessError

let create_args frame jmethod =
  let byte8_count, types = Descriptor.args_of_method (Jmethod.descriptor jmethod) in
  let arg_length = List.length types + byte8_count +
                   (if Jmethod.is_static jmethod then 0 else 1) in
  let args = Array.create ~len:arg_length Null in
  let b8_count = ref 0 in
  List.iteri types ~f:(fun i arg_type ->
      let arg = match arg_type with
        | Descriptor.Byte -> Byte (get_byte @@ stack_pop_exn frame)
        | Descriptor.Short -> Short (get_short @@ stack_pop_exn frame)
        | Descriptor.Char -> Char (get_char @@ stack_pop_exn frame)
        | Descriptor.Int -> Int (get_int @@ stack_pop_exn frame)
        | Descriptor.Float -> Float (get_float @@ stack_pop_exn frame)
        | Descriptor.Long -> b8_count := !b8_count + 1;
          Long (get_long @@ stack_pop_exn frame)
        | Descriptor.Double -> b8_count := !b8_count + 1;
          Double (get_double @@ stack_pop_exn frame)
        | Descriptor.Boolean -> Boolean (get_boolean @@ stack_pop_exn frame)
        | Descriptor.Class cls -> Null
      in Array.set args (arg_length - 1 - i - !b8_count) arg
    );
  args

let op_getstatic frame =
  let index = read_ui16 frame in
  let jfield = get_field frame index in
  let value = Jfield.get_static_value jfield in
  stack_push frame value

(* TODO the value must be of a type that is assignment compatible (JLS §5.2)
   with the field descriptor type*)
let op_putstatic frame =
  let index = read_ui16 frame in
  let jfield = get_field frame index in
  let value = stack_pop_exn frame in
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
  Jfield.set_static_value jfield value;
  if Jfield.is_final jfield then
    let jclass = jfield.Jfield.jclass in
    if jclass <> Frame.current_class frame ||
       current_method_name frame <> "<clinit>" then
      raise IllegalAccessError

let op_getfield frame =
  let jobject = get_object @@ stack_pop_exn frame in
  let index = read_ui16 frame in
  let jfield = get_field frame index in
  validate_protected_field frame (Jobject.jclass jobject) jfield;
  let value = Jobject.get_field_value_exn jobject jfield.Jfield.mid in
  stack_push frame value

let op_putfield frame =
  let index = read_ui16 frame in
  let jfield = get_field frame index in
  let value = stack_pop_exn frame in
  let jobject = get_object @@ stack_pop_exn frame in
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
  validate_protected_field frame (Jobject.jclass jobject) jfield;
  if Accflag.FlagField.is_set jfield.Jfield.access_flags Accflag.FlagField.Final
  then begin
    let jclass = jfield.Jfield.jclass in
    if jclass <> Frame.current_class frame ||
       current_method_name frame <> "<init>" then
      raise IllegalAccessError
  end;
  Jobject.set_field_value_exn jobject jfield.Jfield.mid value

let op_invokevirtual frame = ()

let op_invokespecial frame =
  let index = read_ui16 frame in
  let jmethod = get_special_method_exn frame index in
  let args = create_args frame jmethod in
  let objref = get_object @@ stack_pop_exn frame in
  Array.set args 0 (Object objref);
  Frame.create jmethod args

let op_invokeinterface frame =
  let index = read_ui16 frame in
  let jmethod = get_special_method_exn frame index in
  let args = create_args frame jmethod in
  let cls =
    let jclass = Jmethod.jclass jmethod in
    if Jmethod.name jmethod <> "<init>"
    && not (Jclass.is_interface jclass)
    && FlagClass.is_set jclass.Jclass.access_flags FlagClass.Super then
      let current = current_class frame in
      match current.Jclass.super_class with
      | Some s -> s
      | _ -> raise VirtualMachineError
    else
      jclass
  in
  let memid = Jmethod.memid jmethod in
  let mth =
    match Jclass.find_method cls memid with
    | Some m -> m
    | _ -> let m = if not (Jclass.is_interface cls)
             then Jclass.find_method_in_supper cls memid
             else None
      in match m with
      | Some m -> m
      | _ -> match Jclass.find_method_in_java_long_object cls memid with
        | Some m -> m
        | _ -> match Jclass.find_method_in_interfaces cls memid with
          | Some m -> m
          | _ -> raise AbstractMethodError
  in
  begin
    match stack_pop_exn frame with
    | Array a ->
      validate_protected_method frame a.Jvalue.jclass mth;
      Array.set args 0 (Array a);
    | Object o ->
      validate_protected_method frame o.Jvalue.jclass mth;
      Array.set args 0 (Object o);
    | _ -> raise VirtualMachineError
  end;
  Frame.create mth args

let op_invokedynamic frame = () (* TODO *)

let op_invokestatic frame =
  let index = read_ui16 frame in
  let jmethod = get_static_method_exn frame index in
  let args = create_args frame jmethod in
  Frame.create jmethod args

let op_new frame =
  let index = read_ui16 frame in
  let binary_name = Poolrt.get_class (conspool frame) index in
  let current = current_class frame in
  let jclass = Classloader.resolve_class (current_loader frame)
      ~caller:current.Jclass.name ~name:binary_name
  in
  let obj = Jobject.create jclass in
  stack_push frame (Object obj)

let op_newarray frame =
  let count = get_int @@ stack_pop_exn frame in
  let arr = Jarray.create_primitive (current_class frame) count (read_byte frame) in
  stack_push frame (Array arr)

let op_anewarray frame =
  let index = read_ui16 frame in
  let count = get_int @@ stack_pop_exn frame in
  let binary_name = Poolrt.get_class (conspool frame) index in
  let jclass = Classloader.load_class (current_loader frame) binary_name in
  let arr = Jarray.create_reference jclass count in
  stack_push frame (Array arr)

let op_arraylength frame =
  let arr = get_array @@ stack_pop_exn frame in
  let len = Jarray.length arr in
  stack_push frame (Int len)

let op_athrow frame = ()
let op_checkcast frame = ()
let op_instanceof frame = ()

let op_monitorenter frame = ()
let op_monitorexit frame = ()

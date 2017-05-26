open VMError
open Core.Std
open Accflag

let major_version = 52

let java_lang_object = "java/lang/Object"

let root_class () =
  Jloader.find_class_exn Jloader.bootstrap_loader java_lang_object

let is_interface access_flags =
  FlagClass.is_set access_flags FlagClass.Interface

let create_field jclass field pool =
  let open! Bytecode.Field in
  let name = Poolbc.get_utf8 pool field.name_index in
  let access_flags = field.access_flags in
  let descriptor = Poolbc.get_utf8 pool field.descriptor_index in
  let attrs = field.attributes in
  let mid = MemberID.create name descriptor in
  Jfield.create jclass mid access_flags attrs

let create_method jclass mth pool =
  let open! Bytecode.Method in
  let name = Poolbc.get_utf8 pool mth.name_index in
  let access_flags = mth.access_flags in
  let descriptor = Poolbc.get_utf8 pool mth.descriptor_index in
  let attrs = mth.attributes in
  let mid = MemberID.create name descriptor in
  Jmethod.create jclass mid access_flags attrs

let create_static_fields bytecode =
  let open! Bytecode in
  let statics = MemberID.hashtbl () in
  let pool = bytecode.constant_pool in
  List.iter bytecode.fields ~f:(fun field ->
      let name = Poolbc.get_utf8 pool field.Field.name_index in
      let descriptor = Poolbc.get_utf8 pool field.Field.descriptor_index in
      if FlagField.is_set field.Field.access_flags FlagField.Static then
        let mid = { MemberID.name = name; MemberID.descriptor = descriptor } in
        let value = Jfield.default_value mid in
        Hashtbl.add_exn statics ~key:mid ~data:value
    );
  statics

(* load a none array class from file System *)
let rec load_from_bytecode loader binary_name =
  (* check initiating loader *)
  if Jloader.is_loader loader binary_name then raise LinkageError;
  let open! Bytecode in
  let bytecode = Bytecode.load binary_name in
  (* check major version *)
  if bytecode.major_version > major_version then raise UnsupportedClassVersionError;
  let pool = bytecode.constant_pool in
  let name = Poolbc.get_class pool bytecode.this_class in
  (* check class name *)
  if name <> binary_name then raise NoClassdefFoundError;
  let access_flags = bytecode.access_flags in
  let super_class  = match bytecode.super_class with
    | 0 -> if name <> java_lang_object then (* Only Object has no super class *)
        raise (ClassFormatError "Invalid superclass index")
      else None
    | i -> let jclass = resolve_class loader ~caller:name ~name:(Poolbc.get_class pool i) in
      (* interface's super class must be Object *)
      if is_interface access_flags && (Jclass.name jclass) <> java_lang_object then
        raise (ClassFormatError "Invalid superclass index");
      (* interface can not be super class *)
      if Jclass.is_interface jclass then raise IncompatibleClassChangeError;
      (* super class can not be itself *)
      if name = (Jclass.name jclass) then raise ClassCircularityError;
      Some jclass
  in
  let interfaces = List.map bytecode.interfaces ~f:(fun index ->
      let cls = Poolbc.get_class pool index in
      let jclass = resolve_class loader ~caller:name ~name:cls in
      (* must be interface *)
      if not @@ Jclass.is_interface jclass then raise IncompatibleClassChangeError;
      (* interface can not be itself *)
      if Jclass.name jclass = name then raise ClassCircularityError;
      jclass
    )
  in
  let fields = MemberID.hashtbl () in
  let methods = MemberID.hashtbl () in
  let conspool = Array.create ~len:(Array.length pool) Poolrt.Byte8Placeholder in
  let attributes = bytecode.attributes in
  let static_fields = create_static_fields bytecode in (* 5.4.2 Preparation *)
  let jclass = { Jclass.name = name; Jclass.access_flags = access_flags;
                 Jclass.super_class = super_class; Jclass.interfaces = interfaces;
                 Jclass.fields = fields; Jclass.methods = methods;
                 Jclass.conspool = conspool; Jclass.attributes = attributes;
                 Jclass.loader = loader; Jclass.static_fields = static_fields;
                 Jclass.initialize_state = Jclass.Uninitialized;
               } (* record as defining loader*)
  in
  List.iter bytecode.fields ~f:(fun field ->
      let jfield = create_field jclass field bytecode.constant_pool in
      Hashtbl.add_exn (Jclass.fields jclass) ~key:(Jfield.mid jfield) ~data:jfield
    );
  List.iter bytecode.methods ~f:(fun mth ->
      let jmethod = create_method jclass mth bytecode.constant_pool in
      Hashtbl.add_exn (Jclass.methods jclass) ~key:(Jmethod.mid jmethod) ~data:jmethod
    );
  Jloader.add_class loader jclass; (* record as initiating loader*)
  resovle_pool jclass bytecode.Bytecode.constant_pool;
  jclass

(* Loading Using the Bootstrap Class InnLoader *)
and load_class loader binary_name =
  match Jloader.find_class loader binary_name with
  | Some jclass -> jclass
  | None -> load_from_bytecode loader binary_name

and resolve_class loader ~caller:src_class ~name:binary_name =
  let is_primitive name =
    List.exists ["B";"C";"D";"F";"I";"J";"S";"Z"] ~f:((=) name)
  in
  let resolve () =
    if String.get binary_name 0 = '[' then (* is array *)
      let cmpnt = Descriptor.component_of_class binary_name in
      if is_primitive cmpnt then
        Jclass.create_array Jloader.bootstrap_loader binary_name FlagClass.public_flag
      else
        let cmpnt_class = load_class loader cmpnt in
        Jclass.create_array (Jclass.loader cmpnt_class) binary_name
          (FlagClass.real_acc @@ Jclass.access_flags cmpnt_class)
    else
      load_class loader binary_name
  in
  let jclass = match Jloader.find_class loader binary_name with
    | Some cls -> cls
    | None -> let cls = resolve () in Jloader.add_class loader cls; cls
  in
  let referer_package = Jclass.package_name src_class in
  if not (Jclass.is_public jclass) then begin
    let pkg_target = Jclass.package_name (Jclass.name jclass) in
    if not @@ Jloader.equal loader (Jclass.loader jclass) || referer_package <> pkg_target then
      if not @@ String.contains binary_name '$' then (* inner class access bug? *)
        raise IllegalAccessError
  end;
  jclass

and resovle_pool jclass poolbc =
  let member_arg ci nti =
    let class_name, name, descriptor = Poolbc.get_memberref poolbc ci nti in
    let mid = { MemberID.name = name; MemberID.descriptor = descriptor } in
    class_name, mid
  in
  Array.iteri poolbc ~f:(fun index entry ->
      let new_entry = match entry with
        | Poolbc.Utf8 x -> Poolrt.Utf8 x
        | Poolbc.Integer x -> Poolrt.Integer x
        | Poolbc.Float x -> Poolrt.Float x
        | Poolbc.Long x -> Poolrt.Long x
        | Poolbc.Double x -> Poolrt.Double x
        | Poolbc.Class i -> Poolrt.Class (Poolbc.get_utf8 poolbc i)
        | Poolbc.String i -> Poolrt.String (Poolbc.get_utf8 poolbc i)
        | Poolbc.Fieldref (ci, nti) ->
          let class_name, mid = member_arg ci nti in
          Poolrt.UnresolvedFieldref (class_name, mid)
        | Poolbc.Methodref (ci, nti) ->
          let class_name, mid = member_arg ci nti in
          Poolrt.UnresolvedMethodref (class_name, mid)
        | Poolbc.InterfaceMethodref (ci, nti) ->
          let class_name, mid = member_arg ci nti in
          Poolrt.UnresolvedInterfaceMethodref (class_name, mid)
        | _ -> Poolrt.Byte8Placeholder
      in (Jclass.conspool jclass).(index) <- new_entry
    )

let is_field_accessible src_class jfield =
  let mid = Jfield.mid jfield in
  let target_class = Jfield.jclass jfield in
  let check_private () =
    Option.is_some @@ Jclass.find_field src_class mid
  in
  let check_default () =
    Jclass.package_rt_equal src_class target_class
  in
  let check_protected () =
    if Jclass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  if Jfield.is_public jfield then true
  else if Jfield.is_private jfield then check_private ()
  else if Jfield.is_protected jfield then check_protected ()
  else check_default ()

let is_method_accessible src_class jmethod =
  let mid = Jmethod.mid jmethod in
  let target_class = Jmethod.jclass jmethod in
  let check_private () =
    Option.is_some @@ Jclass.find_method src_class mid
  in
  let check_default () =
    Jclass.package_rt_equal src_class target_class
  in
  let check_protected () =
    if Jclass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  if Jmethod.is_public jmethod then true
  else if Jmethod.is_private jmethod then check_private ()
  else if Jmethod.is_protected jmethod then check_protected ()
  else check_default ()

let rec find_field jclass mid =
  let find_in_interfaces jclass mid =
    let rec aux = function
      | [] -> None
      | head :: tail -> match find_field head mid with
        | Some jfield -> Some jfield
        | None -> aux tail
    in aux @@ Jclass.interfaces jclass
  in
  let find_in_superclass jclass mid =
    match Jclass.super_class jclass with
    | Some cls -> find_field cls mid
    | None -> None
  in
  match Jclass.find_field jclass mid with
  | Some f -> Some f
  | None -> match find_in_interfaces jclass mid with
    | Some f -> Some f
    | None -> match find_in_superclass jclass mid with
      | Some f -> Some f
      | None -> None

let find_method_in_interfaces jclass mid =
  let mss_methods = Jclass.find_mss_methods jclass mid in
  let candidates = List.filter mss_methods ~f:(fun m ->
      not (Jmethod.is_abstract m)
    )
  in
  if List.length candidates = 1 then
    Some (List.hd_exn candidates)
  else if List.length mss_methods > 0 then
    Some (List.hd_exn mss_methods)
  else None

let resolve_field src_class class_name mid =
  let jclass = resolve_class (Jclass.loader src_class)
      ~caller:(Jclass.name src_class) ~name:class_name
  in
  let jfield = match find_field jclass mid with
    | Some jfield -> jfield
    | None -> raise NoSuchFieldError
  in
  if not @@ is_field_accessible src_class jfield then
    raise IllegalAccessError;
  jfield

let resolve_method_of_class src_class class_name mid =
  let resolve_polymorphic jclass mid =
    let classes = Descriptor.classes_of_method mid.MemberID.descriptor in
    List.iter classes ~f:(fun cls ->
        ignore @@ resolve_class (Jclass.loader jclass)
          ~caller:(Jclass.name src_class) ~name:cls
      )
  in
  let rec find_method_in_classes jclass mid =
    match Jclass.find_polymorphic jclass mid with
    | Some m -> resolve_polymorphic jclass mid; Some m
    | _ -> match Jclass.find_method jclass mid with
      | Some m -> Some m
      | _ -> match Jclass.super_class jclass with
        | Some super -> find_method_in_classes super mid
        | _ -> None
  in
  let jclass = resolve_class (Jclass.loader src_class)
      ~caller:(Jclass.name src_class) ~name:class_name
  in
  if Jclass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod =
    match find_method_in_classes jclass mid with
    | Some m -> m
    | _ -> match find_method_in_interfaces jclass mid with
      | Some m -> m
      | _ -> raise NoSuchMethodError
  in
  if not @@ is_method_accessible src_class jmethod then
    raise IllegalAccessError;
  jmethod

let resolve_method_of_interface src_class class_name mid =
  let jclass = resolve_class (Jclass.loader src_class)
      ~caller:(Jclass.name src_class) ~name:class_name
  in
  if not @@ Jclass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match Jclass.find_method jclass mid with
    | Some m -> m
    | _ -> match Jclass.find_method (root_class ()) mid with
      | Some m when Jmethod.is_public m && not (Jmethod.is_static m) -> m
      | _ -> match find_method_in_interfaces jclass mid with
        | Some m -> m
        | _ -> raise NoSuchMethodError
  in
  if not @@ is_method_accessible src_class jmethod then
    raise IllegalAccessError;
  jmethod

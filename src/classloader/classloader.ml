open VMError
open Core.Std
open Accflag

let major_version = 52

let java_lang_object = "java/lang/Object"

module rec InnClass : sig
  type t =
    { name : string;
      access_flags : int;
      super_class  : t option;
      interfaces   : t list;
      fields : (MemberID.t, InnField.t) Hashtbl.t;
      methods : (MemberID.t, InnMethod.t) Hashtbl.t;
      conspool : InnPoolrt.t;
      attributes : Attribute.AttrClass.t list;
      loader  : InnLoader.t;
      static_fields : (MemberID.t, InnValue.t) Hashtbl.t;
    }

  val component : char list -> string
  val package_of_class : string -> string
  val package_rt_equal : t -> t -> bool
  val is_subclass : sub:t -> super:t -> bool
  val is_interface : t -> bool
  val contains_field : t -> MemberID.t -> bool
  val contains_method : t -> MemberID.t -> bool
  val find_field : t -> MemberID.t -> InnField.t option
  val find_method_in_interfaces : t -> MemberID.t -> InnMethod.t option
  val find_method_in_java_long_object : t -> MemberID.t -> InnMethod.t option
  val find_method_of_class : t -> MemberID.t -> InnMethod.t option
  val find_method_of_interface : t -> MemberID.t -> InnMethod.t option
end = struct
  type t =
    { name : string;
      access_flags : int;
      super_class  : t option;
      interfaces   : t list;
      fields : (MemberID.t, InnField.t) Hashtbl.t;
      methods : (MemberID.t, InnMethod.t) Hashtbl.t;
      conspool : InnPoolrt.t;
      attributes : Attribute.AttrClass.t list;
      loader  : InnLoader.t;
      static_fields : (MemberID.t, InnValue.t) Hashtbl.t;
    }

  let rec component = function
    | '['::tail -> component tail
    | 'L'::tail -> String.of_char_list @@ List.slice tail 0 (List.length tail - 1)
    | chrs -> String.of_char_list chrs

  let package_of_class name =
    let cls = component (String.to_list name) in
    let slash = String.rfindi cls ~f:(fun _ c -> c = '/') in
    match slash with
    | Some i -> String.sub cls ~pos:0 ~len:i
    | _ -> "."

  let package_rt_equal class1 class2 =
    InnLoader.equal class1.loader class2.loader &&
    package_of_class class1.name = package_of_class class2.name

  let contains_field jclass mid =
    Option.is_some @@ Hashtbl.find jclass.fields mid

  let contains_method jclass mid =
    Option.is_some @@ Hashtbl.find jclass.methods mid

  let rec is_subclass ~sub ~super =
    sub.name = super.name ||
    match sub.super_class with
    | Some cls -> is_subclass ~sub:cls ~super:super
    | _ -> false

  let is_interface jclass =
    FlagClass.is_set jclass.access_flags FlagClass.Interface

  let rec find_field jclass mid =
    let find_in_interfaces jclass mid =
      let rec aux = function
        | [] -> None
        | head :: tail -> match find_field head mid with
          | Some jfield -> Some jfield
          | None -> aux tail
      in aux jclass.interfaces
    in
    let find_in_superclass jclass mid =
      match jclass.super_class with
      | Some cls -> find_field cls mid
      | None -> None
    in
    match Hashtbl.find jclass.fields mid with
    | Some f -> Some f
    | None -> match find_in_interfaces jclass mid with
      | Some f -> Some f
      | None -> match find_in_superclass jclass mid with
        | Some f -> Some f
        | None -> None

  let find_method_in_interfaces jclass mid =
    let check_acc jmethod =
      FlagMethod.is_not_set_list jmethod.InnMethod.access_flags
        [FlagMethod.Private; FlagMethod.Static]
    in
    let rec aux = function
      | [] -> None
      | hd :: tail -> match Hashtbl.find hd.InnClass.methods mid with
        | Some jmethod when check_acc jmethod -> Some jmethod
        | _ -> match aux hd.InnClass.interfaces with
          | Some jmethod when check_acc jmethod -> Some jmethod
          | _ -> aux tail
    in aux jclass.InnClass.interfaces

  let rec find_method_of_class jclass mid =
    let find_in_superclass jclass mid =
      match jclass.super_class with
      | Some cls -> find_method_of_class cls mid
      | None -> None
    in
    match Hashtbl.find jclass.methods mid with
    | Some m -> Some m
    | None -> match find_in_superclass jclass mid with
      | Some m -> Some m
      | None -> find_method_in_interfaces jclass mid

  let find_method_in_java_long_object jclass mid =
    let cls = InnLoader.find_class_exn jclass.loader java_lang_object in
    match Hashtbl.find cls.methods mid with
    | Some m -> if FlagMethod.is_set m.InnMethod.access_flags FlagMethod.Public
                && FlagMethod.is_not_set m.InnMethod.access_flags FlagMethod.Static
      then Some m else None
    | _ -> None

  let find_method_of_interface jclass mid =
    match Hashtbl.find jclass.methods mid with
    | Some m -> Some m
    | None -> match find_method_in_java_long_object jclass mid with
      | Some m -> Some m
      | None -> find_method_in_interfaces jclass mid
end
and InnField : sig
  type t =
    { jclass        : InnClass.t;
      mid           : MemberID.t;
      access_flags  : int;
      attrs         : Attribute.AttrField.t list;
    }

  val default_value : MemberID.t -> InnValue.t
end = struct
  type t =
    { jclass        : InnClass.t;
      mid           : MemberID.t;
      access_flags  : int;
      attrs         : Attribute.AttrField.t list;
    }

  let default_value mid =
    match Descriptor.type_of_field mid.MemberID.descriptor with
    | Descriptor.Byte -> InnValue.Byte 0
    | Descriptor.Short -> InnValue.Short 0
    | Descriptor.Char -> InnValue.Char 0
    | Descriptor.Int -> InnValue.Int Int32.zero
    | Descriptor.Float -> InnValue.Float Float32.zero
    | Descriptor.Long -> InnValue.Long Int64.zero
    | Descriptor.Double -> InnValue.Double 0.
    | Descriptor.Boolean -> InnValue.Boolean 0
    | Descriptor.Class _ -> InnValue.Null
end
and InnMethod : sig
  type t =
    { jclass        : InnClass.t;
      mid         : MemberID.t;
      access_flags  : int;
      attrs         : Attribute.AttrMethod.t list;
    }

  val is_polymorphic : InnClass.t -> MemberID.t -> t option
end = struct
  type t =
    { jclass        : InnClass.t;
      mid         : MemberID.t;
      access_flags  : int;
      attrs         : Attribute.AttrMethod.t list;
    }

  let is_polymorphic jclass mid =
    if jclass.InnClass.name = "java/lang/invoke/MethodHandle" then
      let new_mid =
        { MemberID.name = mid.MemberID.name;
          MemberID.descriptor = "([Ljava/lang/Object;)Ljava/lang/Object;"
        }
      in
      let jmethod = Hashtbl.find jclass.InnClass.methods new_mid in
      match jmethod with
      | Some m -> if FlagMethod.is_set_list m.InnMethod.access_flags
          [FlagMethod.Varargs; FlagMethod.Native] then Some m else None
      | None -> None
    else None
end
and InnLoader : sig
  type t =
    { name : string;
      classes : (string, InnClass.t) Hashtbl.t;
    }

  val equal : t -> t -> bool
  val find_class : t -> string -> InnClass.t option
  val find_class_exn : t -> string -> InnClass.t
  val is_loader : t -> string -> bool
  val add_class : t -> InnClass.t -> unit
end = struct
  type t =
    { name : string;
      classes : (string, InnClass.t) Hashtbl.t;
    }

  let equal l1 l2 = l1.name = l2.name

  let find_class loader binary_name = Hashtbl.find loader.classes binary_name
  let find_class_exn loader binary_name = Hashtbl.find_exn loader.classes binary_name

  let is_loader loader binary_name = Option.is_some @@ find_class loader binary_name

  let add_class loader jclass =
    let _ = Hashtbl.add loader.classes ~key:jclass.InnClass.name ~data:jclass in ()
end
and InnPoolrt : sig
  type entry =
    | Utf8 of string
    | Integer of int32
    | Float of Float32.t
    | Long of int64
    | Double of float
    | Class of string
    | String of string
    | Fieldref of InnField.t
    | Methodref of InnMethod.t
    | InterfaceMethodref of InnMethod.t
    | UnresolvedFieldref of string * MemberID.t
    | UnresolvedMethodref of string * MemberID.t
    | UnresolvedInterfaceMethodref of string * MemberID.t
    | NameAndType of int * int
    | MethodHandle of string
    | MethodType of string
    | InvokeDynamic of string
    | Byte8Placeholder

  type t = entry array
end = struct
  type entry =
    | Utf8 of string
    | Integer of int32
    | Float of Float32.t
    | Long of int64
    | Double of float
    | Class of string
    | String of string
    | Fieldref of InnField.t
    | Methodref of InnMethod.t
    | InterfaceMethodref of InnMethod.t
    | UnresolvedFieldref of string * MemberID.t
    | UnresolvedMethodref of string * MemberID.t
    | UnresolvedInterfaceMethodref of string * MemberID.t
    | NameAndType of int * int
    | MethodHandle of string
    | MethodType of string
    | InvokeDynamic of string
    | Byte8Placeholder

  type t = entry array
end
and InnValue : sig
  type jbyte = int
  type jshort = int
  type jchar = int
  type jint = int32
  type jlong = int64
  type jfloat = Float32.t
  type jdouble = float
  type jbool = int

  type jobject =
    { jclass : InnClass.t;
      fields : (MemberID.t, InnValue.t) Hashtbl.t;
    }

  type jarray =
    { jclass : InnClass.t;
      values : (InnValue.t) Array.t;
    }

  type t =
    | Byte of jbyte
    | Short of jshort
    | Char of jchar
    | Int of jint
    | Float of jfloat
    | Long of jlong
    | Double of jdouble
    | Boolean of jbool
    | Object of jobject
    | Array of jarray
    | Null
    | ReturnAddress

end = struct
  type jbyte = int
  type jshort = int
  type jchar = int
  type jint = int32
  type jlong = int64
  type jfloat = Float32.t
  type jdouble = float
  type jbool = int

  type jobject =
    { jclass : InnClass.t;
      fields : (MemberID.t, InnValue.t) Hashtbl.t;
    }

  type jarray =
    { jclass : InnClass.t;
      values : (InnValue.t) Array.t;
    }

  type t =
    | Byte of jbyte
    | Short of jshort
    | Char of jchar
    | Int of jint
    | Float of jfloat
    | Long of jlong
    | Double of jdouble
    | Boolean of jbool
    | Object of jobject
    | Array of jarray
    | Null
    | ReturnAddress
end

let bootstrap_loader =
  { InnLoader.name = "_bootstrap";
    InnLoader.classes = Hashtbl.create ~hashable:String.hashable ()
  }

let is_field_accessible src_class jfield =
  let mid = jfield.InnField.mid in
  let target_class = jfield.InnField.jclass in
  let check_private () =
    if InnClass.contains_field src_class mid then true else false
  in
  let check_default () =
    InnClass.package_rt_equal src_class target_class
  in
  let check_protected () =
    if InnClass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  let flags = jfield.InnField.access_flags in
  if FlagField.is_set flags FlagField.Public then true
  else if FlagField.is_set flags FlagField.Private then check_private ()
  else if FlagField.is_set flags FlagField.Protected then check_protected ()
  else check_default ()

let is_method_accessible src_class jmethod =
  let mid = jmethod.InnMethod.mid in
  let target_class = jmethod.InnMethod.jclass in
  let check_private () =
    if InnClass.contains_method src_class mid then true else false
  in
  let check_default () =
    InnClass.package_rt_equal src_class target_class
  in
  let check_protected () =
    if InnClass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  let flags = jmethod.InnMethod.access_flags in
  if FlagMethod.is_set flags FlagMethod.Public then true
  else if FlagMethod.is_set flags FlagMethod.Private then check_private ()
  else if FlagMethod.is_set flags FlagMethod.Protected then check_protected ()
  else check_default ()

(* load a none array class from file System *)
let rec load_from_bytecode loader binary_name =
  let is_interface access_flags =
    FlagClass.is_set access_flags FlagClass.Interface
  in
  let create_field jclass field pool =
    let open! Bytecode.Field in
    let name = Poolbc.get_utf8 pool field.name_index in
    let access_flags = field.access_flags in
    let descriptor = Poolbc.get_utf8 pool field.descriptor_index in
    let attrs = field.attributes in
    let open! InnField in
    { jclass;
      mid = { MemberID.name = name; MemberID.descriptor = descriptor};
      access_flags; attrs;
    }
  in
  let create_method jclass mth pool =
    let open! Bytecode.Method in
    let name = Poolbc.get_utf8 pool mth.name_index in
    let access_flags = mth.access_flags in
    let descriptor = Poolbc.get_utf8 pool mth.descriptor_index in
    let attrs = mth.attributes in
    let open! InnMethod in
    { jclass ; mid = { MemberID.name = name; MemberID.descriptor = descriptor};
      access_flags; attrs;
    }
  in
  let create_static_fields bytecode =
    let open! Bytecode in
    let statics = MemberID.hashtbl () in
    let pool = bytecode.constant_pool in
    List.iter bytecode.fields ~f:(fun field ->
        let name = Poolbc.get_utf8 pool field.Field.name_index in
        let descriptor = Poolbc.get_utf8 pool field.Field.descriptor_index in
        if FlagField.is_set field.Field.access_flags FlagField.Static then
          let mid = { MemberID.name = name; MemberID.descriptor = descriptor } in
          let value = InnField.default_value mid in
          Hashtbl.add_exn statics ~key:mid ~data:value
      );
    statics
  in
  (* check initiating loader *)
  if InnLoader.is_loader loader binary_name then raise LinkageError;
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
      if is_interface access_flags && jclass.InnClass.name <> java_lang_object then
        raise (ClassFormatError "Invalid superclass index");
      (* interface can not be super class *)
      if is_interface jclass.InnClass.access_flags then raise IncompatibleClassChangeError;
      (* super class can not be itself *)
      if name = jclass.InnClass.name then raise ClassCircularityError;
      Some jclass
  in
  let interfaces = List.map bytecode.interfaces ~f:(fun index ->
      let cls = Poolbc.get_class pool index in
      let jclass = resolve_class loader ~caller:name ~name:cls in
      (* must be interface *)
      if not @@ is_interface jclass.InnClass.access_flags then raise IncompatibleClassChangeError;
      (* interface can not be itself *)
      if jclass.InnClass.name = name then raise ClassCircularityError;
      jclass
    )
  in
  let open! InnClass in
  let fields = MemberID.hashtbl () in
  let methods = MemberID.hashtbl () in
  let conspool = Array.create ~len:(Array.length pool) InnPoolrt.Byte8Placeholder in
  let attributes = bytecode.attributes in
  let static_fields = create_static_fields bytecode in
  let jclass = { name; access_flags; super_class; interfaces;
                 fields; methods; conspool; attributes;
                 loader; static_fields} (* record as defining loader*)
  in
  List.iter bytecode.fields ~f:(fun field ->
      let jfield = create_field jclass field bytecode.constant_pool in
      Hashtbl.add_exn jclass.fields ~key:jfield.InnField.mid ~data:jfield
    );
  List.iter bytecode.methods ~f:(fun mth ->
      let jmethod = create_method jclass mth bytecode.constant_pool in
      Hashtbl.add_exn jclass.methods ~key:jmethod.InnMethod.mid ~data:jmethod
    );
  InnLoader.add_class loader jclass; (* record as initiating loader*)
  resovle_pool jclass bytecode.Bytecode.constant_pool;
  jclass

(* Loading Using the Bootstrap Class InnLoader *)
and load_class loader binary_name =
  match InnLoader.find_class loader binary_name with
  | Some jclass -> jclass
  | None -> load_from_bytecode loader binary_name

and resolve_class loader ~caller:src_class ~name:binary_name =
  let is_primitive name =
    List.exists ["B";"C";"D";"F";"I";"J";"S";"Z"] ~f:((=) name)
  in
  let resolve () =
    let open! InnClass in
    if String.get binary_name 0 = '[' then (* is array *)
      let cmpnt = InnClass.component (String.to_list binary_name) in
      if is_primitive cmpnt then
        { name = binary_name; access_flags = FlagClass.public_flag;
          super_class = InnLoader.find_class loader java_lang_object;
          interfaces = []; fields = MemberID.hashtbl ();
          methods = MemberID.hashtbl (); attributes = [];
          conspool = [||]; loader = bootstrap_loader;
          static_fields = MemberID.hashtbl ()
        }
      else
        let cmpnt_class = load_class loader cmpnt in
        { name = binary_name; access_flags = FlagClass.real_acc cmpnt_class.access_flags;
          super_class = InnLoader.find_class loader java_lang_object;
          interfaces = []; fields = MemberID.hashtbl ();
          methods = MemberID.hashtbl (); attributes = [];
          conspool = [||]; loader = cmpnt_class.loader;
          static_fields = MemberID.hashtbl ()
        }
    else
      load_class loader binary_name
  in
  let jclass = match InnLoader.find_class loader binary_name with
    | Some cls -> cls
    | None -> let cls = resolve () in InnLoader.add_class loader cls; cls
  in
  let acc = jclass.InnClass.access_flags in
  let referer_package = InnClass.package_of_class src_class in
  if FlagClass.is_not_set acc FlagClass.Public then begin
    let pkg_target = InnClass.package_of_class jclass.InnClass.name in
    if not @@ InnLoader.equal loader jclass.InnClass.loader || referer_package <> pkg_target then
      if not @@ String.contains binary_name '$' then (* inner class access bug? *)
        raise IllegalAccessError
  end;
  jclass

and resolve_field src_class class_name mid =
  let jclass = resolve_class src_class.InnClass.loader
      ~caller:src_class.InnClass.name ~name:class_name
  in
  let jfield = match InnClass.find_field jclass mid with
    | Some jfield -> jfield
    | None -> raise NoSuchFieldError
  in
  if not @@ is_field_accessible src_class jfield then
    raise IllegalAccessError;
  jfield

and resolve_method_of_class src_class class_name mid =
  let resolve_polymorphic jclass mid =
    let classes = Descriptor.classes_of_method mid.MemberID.descriptor in
    List.iter classes ~f:(fun cls ->
        let _ = resolve_class jclass.InnClass.loader
            ~caller:src_class.InnClass.name ~name:cls in ()
      )
  in
  let jclass = resolve_class src_class.InnClass.loader
      ~caller:src_class.InnClass.name ~name:class_name
  in
  if InnClass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match InnMethod.is_polymorphic jclass mid with
    | Some m -> resolve_polymorphic jclass mid; m
    | None -> match InnClass.find_method_of_class jclass mid with
      | Some m -> m
      | None -> let msg = sprintf "No such method{%s} in class{%s}"
                    (MemberID.to_string mid) class_name
        in raise (NoSuchMethodError msg)
  in
  if not @@ is_method_accessible src_class jmethod then
    raise IllegalAccessError;
  jmethod

and resolve_method_of_interface src_class class_name mid =
  let jclass = resolve_class src_class.InnClass.loader
      ~caller:src_class.InnClass.name ~name:class_name
  in
  if not @@ InnClass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match InnClass.find_method_of_interface jclass mid with
    | Some jmethod -> jmethod
    | None -> let msg = sprintf "No such method{%s} in class{%s}"
                  (MemberID.to_string mid) class_name
      in raise (NoSuchMethodError msg)
  in
  if not @@ is_method_accessible src_class jmethod then
    raise IllegalAccessError;
  jmethod

and resovle_pool jclass poolbc =
  let member_arg ci nti =
    let class_name, name, descriptor = Poolbc.get_memberref poolbc ci nti in
    let mid = { MemberID.name = name; MemberID.descriptor = descriptor } in
    class_name, mid
  in
  Array.iteri poolbc ~f:(fun index entry ->
      let new_entry = match entry with
        | Poolbc.Utf8 x -> InnPoolrt.Utf8 x
        | Poolbc.Integer x -> InnPoolrt.Integer x
        | Poolbc.Float x -> InnPoolrt.Float x
        | Poolbc.Long x -> InnPoolrt.Long x
        | Poolbc.Double x -> InnPoolrt.Double x
        | Poolbc.Class i -> InnPoolrt.Class (Poolbc.get_utf8 poolbc i)
        | Poolbc.String i -> InnPoolrt.String (Poolbc.get_utf8 poolbc i)
        | Poolbc.Fieldref (ci, nti) ->
          let class_name, mid = member_arg ci nti in
          InnPoolrt.UnresolvedFieldref (class_name, mid)
        | Poolbc.Methodref (ci, nti) ->
          let class_name, mid = member_arg ci nti in
          InnPoolrt.UnresolvedMethodref (class_name, mid)
        | Poolbc.InterfaceMethodref (ci, nti) ->
          let class_name, mid = member_arg ci nti in
          InnPoolrt.UnresolvedInterfaceMethodref (class_name, mid)
        | _ -> InnPoolrt.Byte8Placeholder
      in jclass.InnClass.conspool.(index) <- new_entry
    )

let root_class jclass =
  InnLoader.find_class_exn jclass.InnClass.loader java_lang_object

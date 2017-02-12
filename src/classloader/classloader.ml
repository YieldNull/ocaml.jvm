open VMError
open Core.Std
open Accflag

let major_version = 52

let java_lang_object = "java/lang/Object"

let rec component = function
  | '['::tail -> component tail
  | 'L'::tail -> String.of_char_list @@ List.slice tail 0 (List.length tail - 1)
  | chrs -> String.of_char_list chrs

module MemID = struct
  module T = struct
    type t = { name: string; descriptor : string; } [@@deriving sexp, compare]
    let hash t =
      (String.hash t.name) lxor (String.hash t.descriptor)
  end
  include T
  include Hashable.Make(T)

  let to_string t =
    sprintf "%s:%s" t.name t.descriptor
end

module rec Jclass : sig
  type t =
    { name : string;
      access_flags : int;
      super_class  : t option;
      interfaces   : t list;
      fields : (MemID.t, Jfield.t) Hashtbl.t;
      methods : (MemID.t, Jmethod.t) Hashtbl.t;
      conspool : Poolrt.t;
      attributes : Attribute.AttrClass.t list;
      loader  : Loader.t;
    }

  val package_of_name : string -> string
  val package_of_class : t -> string
  val package_rt_equal : t -> t -> bool
  val is_subclass : sub:t -> super:t -> bool
  val is_interface : t -> bool
  val contains_field : t -> MemID.t -> bool
  val contains_method : t -> MemID.t -> bool
  val find_field : t -> MemID.t -> Jfield.t option
  val find_method_of_class : t -> MemID.t -> Jmethod.t option
  val find_method_of_interface : t -> MemID.t -> Jmethod.t option
end = struct
  type t =
    { name : string;
      access_flags : int;
      super_class  : t option;
      interfaces   : t list;
      fields : (MemID.t, Jfield.t) Hashtbl.t;
      methods : (MemID.t, Jmethod.t) Hashtbl.t;
      conspool : Poolrt.t;
      attributes : Attribute.AttrClass.t list;
      loader  : Loader.t;
    }

  let package_of_name name =
    let cls = component (String.to_list name) in
    let slash = String.rfindi cls ~f:(fun _ c -> c = '/') in
    match slash with
    | Some i -> String.sub cls ~pos:0 ~len:i
    | _ -> "."

  let package_of_class jclass = package_of_name jclass.name

  let package_rt_equal class1 class2 =
    Loader.equal class1.loader class2.loader &&
    package_of_name class1.name = package_of_name class2.name

  let contains_field jclass memid =
    Option.is_some @@ Hashtbl.find jclass.fields memid

  let contains_method jclass memid =
    Option.is_some @@ Hashtbl.find jclass.methods memid

  let rec is_subclass ~sub ~super =
    sub.name = super.name ||
    match sub.super_class with
    | Some cls -> is_subclass ~sub:cls ~super:super
    | _ -> false

  let is_interface jclass =
    FlagClass.is_set jclass.access_flags FlagClass.Interface

  let rec find_field jclass memid =
    let find_in_interfaces jclass memid =
      let rec aux = function
        | [] -> None
        | head :: tail -> match find_field head memid with
          | Some jfield -> Some jfield
          | None -> aux tail
      in aux jclass.interfaces
    in
    let find_in_superclass jclass memid =
      match jclass.super_class with
      | Some cls -> find_field cls memid
      | None -> None
    in
    match Hashtbl.find jclass.fields memid with
    | Some f -> Some f
    | None -> match find_in_interfaces jclass memid with
      | Some f -> Some f
      | None -> match find_in_superclass jclass memid with
        | Some f -> Some f
        | None -> None

  let find_method_in_interfaces jclass memid =
    let check_acc jmethod =
      FlagMethod.is_not_set_list jmethod.Jmethod.access_flags
        [FlagMethod.Private; FlagMethod.Static]
    in
    let rec aux = function
      | [] -> None
      | hd :: tail -> match Hashtbl.find hd.Jclass.methods memid with
        | Some jmethod when check_acc jmethod -> Some jmethod
        | _ -> match aux hd.Jclass.interfaces with
          | Some jmethod when check_acc jmethod -> Some jmethod
          | _ -> aux tail
    in aux jclass.Jclass.interfaces

  let rec find_method_of_class jclass memid =
    let find_polymorphic jclass memid =
      if jclass.name = "java/lang/invoke/MethodHandle" then
        let new_memid =
          { MemID.name = memid.MemID.name;
            MemID.descriptor = "([Ljava/lang/Object;)Ljava/lang/Object;"
          }
        in
        let jmethod = Hashtbl.find jclass.methods new_memid in
        match jmethod with
        | Some m -> if FlagMethod.is_set_list m.Jmethod.access_flags
            [FlagMethod.Varargs; FlagMethod.Native] then Some m else None
        | None -> None
      else None
    in
    let find_in_superclass jclass memid =
      match jclass.super_class with
      | Some cls -> find_method_of_class cls memid
      | None -> None
    in
    match find_polymorphic jclass memid with
    | Some m -> Some m
    | None -> match Hashtbl.find jclass.methods memid with
      | Some m -> Some m
      | None -> match find_in_superclass jclass memid with
        | Some m -> Some m
        | None -> find_method_in_interfaces jclass memid

  let find_method_of_interface jclass memid =
    let find_in_object jclass memid =
      let cls = Loader.find_class_exn jclass.loader java_lang_object in
      Hashtbl.find cls.methods memid
    in
    match Hashtbl.find jclass.methods memid with
    | Some m -> Some m
    | None -> match find_in_object jclass memid with
      | Some m -> Some m
      | None -> find_method_in_interfaces jclass memid
end
and Jfield : sig
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : int;
      attrs         : Attribute.AttrField.t list;
    }
end = struct
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : int;
      attrs         : Attribute.AttrField.t list;
    }
end
and Jmethod : sig
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : int;
      attrs         : Attribute.AttrMethod.t list;
    }
end = struct
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : int;
      attrs         : Attribute.AttrMethod.t list;
    }
end
and Loader : sig
  type t =
    { name : string;
      classes : (string, Jclass.t) Hashtbl.t;
    }

  val equal : t -> t -> bool
  val find_class : t -> string -> Jclass.t option
  val find_class_exn : t -> string -> Jclass.t
  val is_loader : t -> string -> bool
  val add_class : t -> Jclass.t -> unit
  val add_class_exn : t -> Jclass.t -> unit
end = struct
  type t =
    { name : string;
      classes : (string, Jclass.t) Hashtbl.t;
    }

  let equal l1 l2 = l1.name = l2.name

  let find_class loader binary_name = Hashtbl.find loader.classes binary_name
  let find_class_exn loader binary_name = Hashtbl.find_exn loader.classes binary_name

  let is_loader loader binary_name = Option.is_some @@ find_class loader binary_name

  let add_class loader jclass =
    let _ = Hashtbl.add loader.classes ~key:jclass.Jclass.name ~data:jclass in ()

  let add_class_exn loader jclass =
    Hashtbl.add_exn loader.classes ~key:jclass.Jclass.name ~data:jclass
end
and Poolrt : sig
  type entry =
    | Utf8 of string
    | Integer of int32
    | Float of float
    | Long of int64
    | Double of float
    | Class of Jclass.t
    | String of string
    | Fieldref of Jfield.t
    | Methodref of Jmethod.t
    | InterfaceMethodref of Jmethod.t
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
    | Float of float
    | Long of int64
    | Double of float
    | Class of Jclass.t
    | String of string
    | Fieldref of Jfield.t
    | Methodref of Jmethod.t
    | InterfaceMethodref of Jmethod.t
    | NameAndType of int * int
    | MethodHandle of string
    | MethodType of string
    | InvokeDynamic of string
    | Byte8Placeholder

  type t = entry array
end

let bootstrap_loader =
  { Loader.name = "_bootstrap";
    Loader.classes = Hashtbl.create ~hashable:String.hashable ()
  }

let create_membertbl () =
  Hashtbl.create ~hashable:MemID.hashable ()

let is_field_accessible src_class jfield =
  let memid = jfield.Jfield.memid in
  let target_class = jfield.Jfield.jclass in
  let check_private () =
    if Jclass.contains_field src_class memid then true else false
  in
  let check_default () =
    Jclass.package_rt_equal src_class target_class
  in
  let check_protected () =
    if Jclass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  let flags = jfield.Jfield.access_flags in
  if FlagField.is_set flags FlagField.Public then true
  else if FlagField.is_set flags FlagField.Private then check_private ()
  else if FlagField.is_set flags FlagField.Protected then check_protected ()
  else check_default ()

let is_method_accessible src_class jmethod =
  let memid = jmethod.Jmethod.memid in
  let target_class = jmethod.Jmethod.jclass in
  let check_private () =
    if Jclass.contains_method src_class memid then true else false
  in
  let check_default () =
    Jclass.package_rt_equal src_class target_class
  in
  let check_protected () =
    if Jclass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  let flags = jmethod.Jmethod.access_flags in
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
    let open! Jfield in
    { jclass ; memid = { MemID.name = name; MemID.descriptor = descriptor};
      access_flags; attrs;
    }
  in
  let create_method jclass mth pool =
    let open! Bytecode.Method in
    let name = Poolbc.get_utf8 pool mth.name_index in
    let access_flags = mth.access_flags in
    let descriptor = Poolbc.get_utf8 pool mth.descriptor_index in
    let attrs = mth.attributes in
    let open! Jmethod in
    { jclass ; memid = { MemID.name = name; MemID.descriptor = descriptor};
      access_flags; attrs;
    }
  in
  (* check initiating loader *)
  if Loader.is_loader loader binary_name then raise LinkageError;
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
    | i -> let jclass = resolve_class loader name (Poolbc.get_class pool i) in
      (* interface's super class must be Object *)
      if is_interface access_flags && jclass.Jclass.name <> java_lang_object then
        raise (ClassFormatError "Invalid superclass index");
      (* interface can not be super class *)
      if is_interface jclass.Jclass.access_flags then raise IncompatibleClassChangeError;
      (* super class can not be itself *)
      if name = jclass.Jclass.name then raise ClassCircularityError;
      Some jclass
  in
  let interfaces = List.map bytecode.interfaces ~f:(fun index ->
      let cls = Poolbc.get_class pool index in
      let jclass = resolve_class loader name cls in
      (* must be interface *)
      if not @@ is_interface jclass.Jclass.access_flags then raise IncompatibleClassChangeError;
      (* interface can not be itself *)
      if jclass.Jclass.name = name then raise ClassCircularityError;
      jclass
    )
  in
  let open! Jclass in
  let fields = create_membertbl () in
  let methods = create_membertbl () in
  let conspool = Array.create ~len:(Array.length pool) Poolrt.Byte8Placeholder in
  let attributes = bytecode.attributes in
  let jclass = { name; access_flags; super_class; interfaces;
                 fields; methods; conspool; attributes;
                 loader;} (* record as defining loader*)
  in
  List.iter bytecode.fields ~f:(fun field ->
      let jfield = create_field jclass field bytecode.constant_pool in
      Hashtbl.add_exn jclass.fields ~key:jfield.Jfield.memid ~data:jfield
    );
  List.iter bytecode.methods ~f:(fun mth ->
      let jmethod = create_method jclass mth bytecode.constant_pool in
      Hashtbl.add_exn jclass.methods ~key:jmethod.Jmethod.memid ~data:jmethod
    );
  Loader.add_class loader jclass; (* record as initiating loader*)
  resovle_pool jclass bytecode.Bytecode.constant_pool;
  jclass

(* Loading Using the Bootstrap Class Loader *)
and load_class loader binary_name =
  match Loader.find_class loader binary_name with
  | Some jclass -> jclass
  | None -> load_from_bytecode loader binary_name

and resolve_class loader src_class binary_name =
  let is_array name = String.get name 0 = '[' in
  let is_primitive name =
    List.exists ["B";"C";"D";"F";"I";"J";"S";"Z"] ~f:((=) name)
  in
  let resolve () =
    let open! Jclass in
    if is_array binary_name then
      let cmpnt = component (String.to_list binary_name) in
      if is_primitive cmpnt then
        { name = binary_name; access_flags = FlagClass.public_flag;
          super_class = Loader.find_class loader java_lang_object;
          interfaces = []; fields = create_membertbl ();
          methods = create_membertbl (); attributes = [];
          conspool = [||]; loader = bootstrap_loader;
        }
      else
        let cmpnt_class = load_class loader cmpnt in
        { name = binary_name; access_flags = FlagClass.real_acc cmpnt_class.access_flags;
          super_class = Loader.find_class loader java_lang_object;
          interfaces = []; fields = create_membertbl ();
          methods = create_membertbl (); attributes = [];
          conspool = [||]; loader = cmpnt_class.loader;
        }
    else
      load_class loader binary_name
  in
  let jclass = match Loader.find_class loader binary_name with
    | Some cls -> cls
    | None -> let cls = resolve () in Loader.add_class loader cls; cls
  in
  let acc = jclass.Jclass.access_flags in
  let referer_package = Jclass.package_of_name src_class in
  if FlagClass.is_not_set acc FlagClass.Public then begin
    let pkg_target = Jclass.package_of_name jclass.Jclass.name in
    if not @@ Loader.equal loader jclass.Jclass.loader || referer_package <> pkg_target then
      if not @@ String.contains binary_name '$' then (* inner class access bug? *)
        raise IllegalAccessError
  end;
  jclass

and resolve_field src_class class_name memid =
  let jclass = resolve_class src_class.Jclass.loader src_class.Jclass.name class_name in
  let jfield = match Jclass.find_field jclass memid with
    | Some jfield -> jfield
    | None -> raise NoSuchFieldError
  in
  if not @@ is_field_accessible src_class jfield then
    raise IllegalAccessError;
  jfield

and resolve_method_of_class src_class class_name memid =
  let jclass = resolve_class src_class.Jclass.loader src_class.Jclass.name class_name in
  if Jclass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match Jclass.find_method_of_class jclass memid with
    | Some jmethod -> jmethod
    | None -> let msg = sprintf "No such method{%s} in class{%s}"
                  (MemID.to_string memid) class_name
      in raise (NoSuchMethodError msg)
  in
  if not @@ is_method_accessible src_class jmethod then
    raise IllegalAccessError;
  jmethod

and resolve_method_of_interface src_class class_name memid =
  let jclass = resolve_class src_class.Jclass.loader src_class.Jclass.name class_name in
  if not @@ Jclass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match Jclass.find_method_of_interface jclass memid with
    | Some jmethod -> jmethod
    | None -> let msg = sprintf "No such method{%s} in class{%s}"
                  (MemID.to_string memid) class_name
      in raise (NoSuchMethodError msg)
  in
  if not @@ is_method_accessible src_class jmethod then
    raise IllegalAccessError;
  jmethod

and resovle_pool jclass poolbc =
  let member_arg ci nti =
    let class_name, name, descriptor = Poolbc.get_memberref poolbc ci nti in
    let memid = { MemID.name = name; MemID.descriptor = descriptor } in
    class_name, memid
  in
  let loader = jclass.Jclass.loader in
  Array.iteri poolbc ~f:(fun index entry ->
      let new_entry = match entry with
        | Poolbc.Utf8 x -> Poolrt.Utf8 x
        | Poolbc.Integer x -> Poolrt.Integer x
        | Poolbc.Float x -> Poolrt.Float x
        | Poolbc.Long x -> Poolrt.Long x
        | Poolbc.Double x -> Poolrt.Double x
        | Poolbc.Class i -> Poolrt.Class (
            resolve_class loader jclass.Jclass.name (Poolbc.get_utf8 poolbc i)
          )
        | Poolbc.String i -> Poolrt.String (Poolbc.get_utf8 poolbc i)
        | Poolbc.Fieldref (ci, nti) ->
          let class_name, memid = member_arg ci nti in
          let jfield = resolve_field jclass class_name memid in
          Poolrt.Fieldref jfield
        | Poolbc.Methodref (ci, nti) ->
          let class_name, memid = member_arg ci nti in
          let jmethod = resolve_method_of_class jclass class_name memid in
          Poolrt.Methodref jmethod
        | Poolbc.InterfaceMethodref (ci, nti) ->
          let class_name, memid = member_arg ci nti in
          let jmethod = resolve_method_of_interface jclass class_name memid in
          Poolrt.InterfaceMethodref jmethod
        | _ -> Poolrt.Byte8Placeholder
      in jclass.Jclass.conspool.(index) <- new_entry
    )

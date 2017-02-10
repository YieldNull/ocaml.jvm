open VMError
open Core.Std

let major_version = 52

let java_lang_object = "java/lang/Object"

module MemID = struct
  module T = struct
    type t = { name: string; descriptor : string; } [@@deriving sexp, compare]
    let hash t =
      (String.hash t.name) lxor (String.hash t.descriptor)
  end
  include T
  include Hashable.Make(T)
end

module rec Jclass : sig
  type t =
    { name : string;
      access_flags : Accflag.t list;
      super_class  : t option;
      interfaces   : t list;
      fields : (MemID.t, Jfield.t) Hashtbl.t;
      methods : (MemID.t, Jmethod.t) Hashtbl.t;
      conspool : conspool array;
      attributes : Attribute.AttrClass.t list;
      loader  : Loader.t;
    }
  and conspool =
    | Utf8 of string
    | Integer of int32
    | Float of float
    | Long of int64
    | Double of float
    | Class of t
    | String of string
    | Fieldref of Jfield.t
    | Methodref of Jmethod.t
    | InterfaceMethodref of string
    | NameAndType of int * int
    | MethodHandle of string
    | MethodType of string
    | InvokeDynamic of string
    | Byte8Placeholder

  val package_of_name : string -> string
  val package_equal : t -> t -> bool
  val is_subclass : sub:t -> super:t -> bool
  val is_interface : t -> bool
  val contains_field : t -> MemID.t -> bool
  val find_field : t -> MemID.t -> Jfield.t option
  val find_method_of_class : t -> MemID.t -> Jmethod.t option
  val find_method_of_interface : t -> MemID.t -> Jmethod.t option
end = struct
  type t =
    { name : string;
      access_flags : Accflag.t list;
      super_class  : t option;
      interfaces   : t list;
      fields : (MemID.t, Jfield.t) Hashtbl.t;
      methods : (MemID.t, Jmethod.t) Hashtbl.t;
      conspool : conspool array;
      attributes : Attribute.AttrClass.t list;
      loader  : Loader.t;
    }
  and conspool =
    | Utf8 of string
    | Integer of int32
    | Float of float
    | Long of int64
    | Double of float
    | Class of t
    | String of string
    | Fieldref of Jfield.t
    | Methodref of Jmethod.t
    | InterfaceMethodref of string
    | NameAndType of int * int
    | MethodHandle of string
    | MethodType of string
    | InvokeDynamic of string
    | Byte8Placeholder

  let package_of_name name =
    let slash = String.rfindi name ~f:(fun _ c -> c = '/') in
    match slash with
    | Some i -> String.sub name ~pos:0 ~len:i
    | _ -> "."

  let package_equal class1 class2 =
    Loader.equal class1.loader class2.loader &&
    package_of_name class1.name = package_of_name class2.name

  let contains_field jclass memid =
    Option.is_some @@ Hashtbl.find jclass.fields memid

  let rec is_subclass ~sub ~super =
    sub.name = super.name ||
    match sub.super_class with
    | Some cls -> is_subclass ~sub:cls ~super:super
    | _ -> false

  let is_interface jclass =
    List.exists jclass.access_flags ~f:((=) Accflag.Interface)

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
    (* maximally-specific superinterface method *)
    let find_mssm jclass memid =
      let rec find_single cls =
        match Hashtbl.find cls.methods memid with
        | Some jmethod -> if List.exists jmethod.Jmethod.access_flags
            ~f:(fun acc -> acc = Accflag.Private || acc = Accflag.Static)
          then None else Some jmethod
        | None -> match cls.super_class with
          | Some super -> find_single super
          | None -> None
      in
      List.filter_map jclass.interfaces ~f:find_single
    in
    let mssms = find_mssm jclass memid in
    let mssm = List.find_map mssms ~f:(fun jmethod ->
        if List.exists jmethod.Jmethod.access_flags ~f:((=) Accflag.Abstract) then None
        else Some jmethod
      ) in match mssm with
    | Some m -> Some m
    | None -> List.hd mssms

  let rec find_method_of_class jclass memid =
    let find_in_superclass jclass memid =
      match jclass.super_class with
      | Some cls -> find_method_of_class cls memid
      | None -> None
    in
    match Hashtbl.find jclass.methods memid with
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
      access_flags  : Accflag.t list;
      attrs         : Attribute.AttrField.t list;
    }
end = struct
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : Accflag.t list;
      attrs         : Attribute.AttrField.t list;
    }
end
and Jmethod : sig
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : Accflag.t list;
      attrs         : Attribute.AttrMethod.t list;
    }
end = struct
  type t =
    { jclass        : Jclass.t;
      memid         : MemID.t;
      access_flags  : Accflag.t list;
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

let bootstrap_loader =
  { Loader.name = "_bootstrap";
    Loader.classes = Hashtbl.create ~hashable:String.hashable ()
  }

let create_membertbl () =
  Hashtbl.create ~hashable:MemID.hashable ()

(* load a none array class from file System *)
let rec load_from_bytecode loader binary_name =
  let is_interface access_flags =
    List.exists access_flags ~f:((=) Accflag.Interface)
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
  let package = Jclass.package_of_name name in
  let access_flags = bytecode.access_flags in
  let super_class  = match bytecode.super_class with
    | 0 -> if name <> java_lang_object then (* Only Object has no super class *)
        raise (ClassFormatError "Invalid superclass index")
      else None
    | i -> let jclass = resolve_class loader package (Poolbc.get_class pool i) in
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
      let jclass = resolve_class loader package cls in
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
  let conspool = Array.create ~len:(Array.length pool) Byte8Placeholder in
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
  (* record as initiating loader*)
  Loader.add_class_exn loader jclass;
  jclass

(* Loading Using the Bootstrap Class Loader *)
and load_class loader binary_name =
  match Loader.find_class loader binary_name with
  | Some jclass -> jclass
  | None -> load_from_bytecode loader binary_name

and resolve_class loader referer_package binary_name =
  let is_array name = String.get name 0 = '[' in
  let is_primitive name =
    List.exists ["B";"C";"D";"F";"I";"J";"S";"Z"] ~f:((=) name)
  in
  let rec component name_char_list = match name_char_list with
    | '['::tail -> component tail
    | chrs -> String.of_char_list chrs
  in
  let resolve () =
    let open! Jclass in
    if is_array binary_name then
      let cmpnt = component (String.to_list binary_name) in
      if is_primitive cmpnt then
        { name = binary_name; access_flags = [Accflag.Public];
          super_class = Loader.find_class loader java_lang_object;
          interfaces = []; fields = create_membertbl ();
          methods = create_membertbl (); attributes = [];
          conspool = [||]; loader = bootstrap_loader;
        }
      else
        let cmpnt_class = load_class loader cmpnt in
        { name = binary_name; access_flags = cmpnt_class.access_flags;
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
  if not (List.exists acc ~f:((=) Accflag.Public)) then begin
    let pkg_target = Jclass.package_of_name jclass.Jclass.name in
    if not @@ Loader.equal loader jclass.Jclass.loader || referer_package <> pkg_target then
      raise IllegalAccessError
  end;
  jclass

let member_accessible src_class target_class memid acc =
  let check_private () =
    if Jclass.contains_field src_class memid then true else false
  in
  let check_default () =
    Jclass.package_equal src_class target_class
  in
  let check_protected () =
    if Jclass.is_subclass ~sub:src_class ~super:target_class then true
    else check_default ()
  in
  let rec check = function
    | [] -> check_default ()
    | h :: t -> match h with
      | Accflag.Public -> true
      | Accflag.Private -> check_private ()
      | Accflag.Protected -> check_protected ()
      | _ -> check t
  in check acc

let resolve_field src_class class_name memid =
  let jclass = load_class src_class.Jclass.loader class_name in
  let jfield = match Jclass.find_field jclass memid with
    | Some jfield -> jfield
    | None -> raise NoSuchFieldError
  in
  if not @@ member_accessible src_class jclass memid jfield.Jfield.access_flags then
    raise IllegalAccessError;
  jfield

let resolve_method_of_class src_class class_name memid =
  let jclass = load_class src_class.Jclass.loader class_name in
  if Jclass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match Jclass.find_method_of_class jclass memid with
    | Some jmethod -> jmethod
    | None -> raise NoSuchMethodError
  in
  if not @@ member_accessible src_class jclass memid jmethod.Jmethod.access_flags then
    raise IllegalAccessError;
  jmethod

let resolve_method_of_interface src_class class_name memid =
  let jclass = load_class src_class.Jclass.loader class_name in
  if not @@ Jclass.is_interface jclass then raise IncompatibleClassChangeError;
  let jmethod = match Jclass.find_method_of_interface jclass memid with
    | Some jmethod -> jmethod
    | None -> raise NoSuchMethodError
  in
  if not @@ member_accessible src_class jclass memid jmethod.Jmethod.access_flags then
    raise IllegalAccessError;
  jmethod

open Core.Std
open Accflag

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
      mutable initialized : bool;
    }

  val package_rt_equal : t -> t -> bool
  val is_subclass : sub:t -> super:t -> bool
  val is_interface : t -> bool
  val find_method : t -> MemberID.t -> InnMethod.t option
  val find_field : t -> MemberID.t -> InnField.t option
  val find_mss_methods : t -> MemberID.t -> InnMethod.t list
end
and InnField: sig
  type t =
    { jclass        : InnClass.t;
      mid           : MemberID.t;
      access_flags  : int;
      attrs         : Attribute.AttrField.t list;
    }
  val default_value : MemberID.t -> InnValue.t
end
and InnMethod : sig
  type t =
    { jclass        : InnClass.t;
      mid           : MemberID.t;
      access_flags  : int;
      attrs         : Attribute.AttrMethod.t list;
    }
  val is_polymorphic : InnClass.t -> MemberID.t -> t option
end
and InnLoader : sig
  type t =
    { name : string;
      classes : (string, InnClass.t) Hashtbl.t;
    }
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
end

val bootstrap_loader : InnLoader.t

val root_class : unit -> InnClass.t

(* load a class from bytecode. do not use it for resolving entries in constant_pool *)
val load_class : InnLoader.t -> string -> InnClass.t

(* resolve a class in constant_pool *)
val resolve_class : InnLoader.t -> caller:string -> name:string -> InnClass.t

(* resolve a field in constant_pool *)
val resolve_field : InnClass.t -> string -> MemberID.t -> InnField.t

(* resolve a method of class in constant_pool *)
val resolve_method_of_class : InnClass.t -> string -> MemberID.t -> InnMethod.t

(* resolve a method of interface in constant_pool *)
val resolve_method_of_interface : InnClass.t -> string -> MemberID.t -> InnMethod.t

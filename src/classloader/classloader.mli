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
    }
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
    | Class of InnObject.obj
    | String of InnObject.obj
    | Fieldref of InnField.t
    | Methodref of InnMethod.t
    | InterfaceMethodref of InnMethod.t
    | UnresolvedString of string
    | UnresolvedClass of string
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
  type jbool = bool

  type t =
    | Byte of jbyte
    | Short of jshort
    | Char of jchar
    | Int of jint
    | Float of jfloat
    | Long of jlong
    | Double of jdouble
    | Boolean of jbool
    | Reference of InnObject.t
    | ReturnAddress
end
and InnObject : sig
  type obj =
    { jclass : InnClass.t;
      fields : (MemberID.t, InnValue.t) Hashtbl.t;
    }

  type arr =
    { jclass : InnClass.t option;
      values : (InnValue.t) Array.t;
    }

  type t =
    | Obj of obj
    | Arr of arr
    | Null
end

val bootstrap_loader : InnLoader.t

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

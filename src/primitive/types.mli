open Core
open Accflag
open Attribute

module rec InnClass : sig
  type state =
    | Uninitialized
    | Initialing
    | Initialized

  type vmethod =
    | AccessibleVMethod of InnMethod.t
    | InaccessibleVMethod (* inaccessible*)

  type t =
    { name : string;
      access_flags  : int;
      super_class   : t option;
      interfaces    : t list;
      conspool      : InnPoolrt.t;
      attributes    : AttrClass.t list;
      loader        : InnLoader.t;
      fields        : (MemberID.t, InnField.t) Hashtbl.t;
      static_fields : (MemberID.t, InnValue.t) Hashtbl.t;
      static_methods  : (MemberID.t, InnMethod.t) Hashtbl.t;
      virtual_methods : (MemberID.t, InnMethod.t) Hashtbl.t;
      special_methods : (MemberID.t, InnMethod.t) Hashtbl.t;
      vtable        : vmethod array;
      itables       : (string, InnMethod.t array) Hashtbl.t;
      mutable initialize_state : state;
    }
end
and InnField : sig
  type t =
    { jclass        : InnClass.t;
      mid           : MemberID.t;
      access_flags  : int;
      attrs         : AttrField.t list;
    }
end

and InnMethod : sig
  type t =
    { jclass        : InnClass.t;
      mid           : MemberID.t;
      access_flags  : int;
      attrs         : AttrMethod.t list;
      table_index   : int;
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

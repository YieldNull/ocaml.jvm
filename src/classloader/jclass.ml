open Core.Std

type jclass =
  { name : string;
    access_flags : Accflag.t list;
    super_class  : jclass option;
    interfaces   : jclass list;
    fields : jfield list;
    methods : jmethod list;
    conspool : conspool array;
    attributes : Attribute.AttrClass.t list;
    loader_name : string;
  }
and jfield =
  { field_name          : string;
    field_descriptor    : string;
    field_access_flags  : Accflag.t list;
    field_attrs         : Attribute.AttrField.t list;
  }
and jmethod =
  { method_name          : string;
    method_descriptor    : string;
    method_access_flags  : Accflag.t list;
    method_attrs         : Attribute.AttrMethod.t list;
  }
and conspool =
  | Utf8 of string
  | Integer of int32
  | Float of float
  | Long of int64
  | Double of float
  | Class of jclass
  | String of string
  | Fieldref of jclass * jfield
  | Methodref of jclass * jmethod
  | InterfaceMethodref of string
  | NameAndType of int * int
  | MethodHandle of string
  | MethodType of string
  | InvokeDynamic of string
  | Byte8Placeholder

type t = jclass

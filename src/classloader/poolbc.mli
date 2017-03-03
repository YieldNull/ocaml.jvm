type elt =
  | Utf8                of string
  | Integer             of int32
  | Float               of Float32.t
  | Long                of int64
  | Double              of float
  | Class               of int (* utf8_index *)
  | String              of int (* utf8_index *)
  | Fieldref            of int * int (* class_index, name_and_type_index *)
  | Methodref           of int * int (* class_index, name_and_type_index *)
  | InterfaceMethodref  of int * int (* class_index, name_and_type_index *)
  | NameAndType         of int * int (* name_index, descriptor_index *)
  | MethodHandle        of int * int (* reference_kind, reference_index *)
  | MethodType          of int (* descriptor_index *)
  | InvokeDynamic       of int * int (* bootstrap_method_attr_index, name_and_type_index *)
  | Byte8Placeholder

type t = elt array

val create : BatInnerIO.input -> t

val get_utf8 : t -> int -> string

val get_class : t -> int -> string

val get_string : t -> int -> string

val get_memberref : t -> int -> int -> string * string * string

val get_method_handle : t -> int -> int * int

val get_method_type : t -> int -> string

val get_invoke_dynamic : t -> int -> int * int

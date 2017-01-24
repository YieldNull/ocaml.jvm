type elt =
  | Utf8                of string
  | Integer             of int32
  | Float               of float
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

val get_integer : t -> int -> int32

val get_float : t -> int -> float

val get_long : t -> int -> int64

val get_double : t -> int -> float

val get_class : t -> int -> string

val get_string : t -> int -> string

val get_name_and_type : t -> int -> int * int

val get_memberref : t -> int -> int * int

val get_method_handle : t -> int -> int * int

val get_method_type : t -> int -> string

val get_invoke_dynamic : t -> int -> int * int

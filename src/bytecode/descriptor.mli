open Core.Std

type t =
  | Byte
  | Short
  | Char
  | Int
  | Float
  | Long
  | Double
  | Boolean
  | Class of string

val type_of_field : string -> t
val classes_of_method : string -> string list
val args_of_method : string -> int * t list
val component_of_class : string -> string

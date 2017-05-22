open Core.Std

type field =
  | Byte
  | Short
  | Char
  | Int
  | Float
  | Long
  | Double
  | Boolean
  | Class of string

val type_of_field : string -> field
val classes_of_method : string -> string list
val args_of_method : string -> int * field list

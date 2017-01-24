type t =
  | Public | Private   | Protected | Static
  | Final  | Super     | Volatile  | Transient
  | Native | Interface | Abstract  | Strict
  | Synthetic | Annotation | Enum

val parse : int -> t list

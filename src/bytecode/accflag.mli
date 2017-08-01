open Core

module FlagClass : sig
  type t =
    | Public | Final | Super | Interface
    | Abstract | Synthetic | Annotation | Enum

  val is_set : int -> t -> bool
  val is_not_set : int -> t -> bool
  val is_set_list : int -> t list -> bool
  val is_not_set_list : int -> t list -> bool

  val public_flag : int
  val real_acc : int -> int
end

module FlagField : sig
  type t =
    | Public | Private | Protected | Static
    | Final | Volatile | Transient | Synthetic | Enum

  val is_set : int -> t -> bool
  val is_not_set : int -> t -> bool
  val is_set_list : int -> t list -> bool
  val is_not_set_list : int -> t list -> bool
end

module FlagMethod : sig
  type t =
    | Public | Private | Protected | Static
    | Final | Synchronized | Bridge | Varargs
    | Native | Abstract | Strict | Synthetic

  val is_set : int -> t -> bool
  val is_not_set : int -> t -> bool
  val is_set_list : int -> t list -> bool
  val is_not_set_list : int -> t list -> bool
end

open Core
open Attribute

type t

val create : Jmethod.t -> Jvalue.t array -> t

val is_native : t -> bool

val stack_push : t -> Jvalue.t -> unit

val stack_pop_exn : t -> Jvalue.t

val stack_top_exn : t -> Jvalue.t

val localvar_get : t -> int -> Jvalue.t

val localvar_set : t -> int -> Jvalue.t -> unit

val current_class : t -> Jclass.t

val current_loader : t -> Types.InnLoader.t

val current_method : t -> Jmethod.t

val current_method_name : t -> string

val conspool : t -> Poolrt.t

val read_byte : t -> int

val read_signed_byte : t -> int

val read_ui16 : t -> int

val read_i16 : t -> int

val set_pc_offset : t -> int -> unit

val is_end_of_codes : t -> bool

val set_end_of_codes : t -> unit

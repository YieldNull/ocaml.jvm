open Types
open Attribute

type t = Types.InnField.t

val create : InnClass.t -> MemberID.t -> int -> AttrField.t list -> t

val jclass : t -> InnClass.t

val mid : t -> MemberID.t

val name : t -> string

val descriptor : t -> string

val access_flags : t -> int

val attrs : t -> AttrField.t list

val is_final : t -> bool

val is_static : t -> bool

val is_public : t -> bool

val is_protected : t -> bool

val is_private : t -> bool

val default_value : MemberID.t -> InnValue.t

val get_static_value : t -> InnValue.t

val set_static_value : t -> InnValue.t -> unit

val constant_value : t -> int option

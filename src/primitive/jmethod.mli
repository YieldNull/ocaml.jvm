open Types
open Attribute

type t = Types.InnMethod.t

val create : InnClass.t -> MemberID.t -> int -> AttrMethod.t list -> t

val jclass : t -> InnClass.t

val mid : t -> MemberID.t

val name : t -> string

val descriptor : t -> string

val access_flags : t -> int

val attrs : t -> AttrMethod.t list

val table_index : t -> int

val is_static : t -> bool

val is_public : t -> bool

val is_protected : t -> bool

val is_default : t -> bool

val is_private : t -> bool

val is_abstract : t -> bool

val is_native : t -> bool

val equal : t -> t -> bool

open Classloader

type t = Classloader.InnMethod.t

val memid : t -> MemberID.t

val jclass : t -> InnClass.t

val name : t -> string

val descriptor : t -> string

val attrs : t -> Attribute.AttrMethod.t list

val access_flags : t -> int

val is_protected : t -> bool

val is_static : t -> bool

val is_abstract : t -> bool

val is_private : t -> bool

val is_public : t -> bool

val equal : t -> t -> bool

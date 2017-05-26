open Core.Std

type t = { name: string; descriptor : string; }
include Hashable.S with type t := t

val create : string -> string -> t

val name : t -> string

val descriptor : t -> string

val hashtbl : unit -> (t,'b) Hashtbl.t

val to_string : t -> string

val cinit : t

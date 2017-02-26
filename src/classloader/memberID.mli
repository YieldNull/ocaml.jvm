open Core.Std

type t = { name: string; descriptor : string; }
include Hashable.S with type t := t

val hashtbl : unit -> (t,'b) Hashtbl.t
val to_string : t -> string

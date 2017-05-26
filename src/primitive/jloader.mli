open Core.Std
open Types

type t = Types.InnLoader.t

val bootstrap_loader : t

val classes : t -> (string, InnClass.t) Hashtbl.t

val name : t -> string

val equal : t -> t -> bool

val find_class : t -> string -> InnClass.t option

val find_class_exn : t -> string -> InnClass.t

val is_loader : t -> string -> bool

val add_class : t -> InnClass.t -> unit

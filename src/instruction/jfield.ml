open Core.Std
open Accflag
open VMError

include Classloader.InnField

let is_static jfield = FlagField.is_set jfield.access_flags FlagField.Static

let get_static_value jfield =
  let static_fields = jfield.jclass.Jclass.static_fields in
  if is_static jfield then
    Hashtbl.find_exn static_fields jfield.mid
  else
    raise IncompatibleClassChangeError

let set_static_value jfield value =
  let static_fields = jfield.jclass.Jclass.static_fields in
  if is_static jfield then
    Hashtbl.set static_fields ~key:jfield.mid ~data:value
  else
    raise IncompatibleClassChangeError

open Core.Std
open Accflag
open VMError
open Attribute

include Classloader.InnField

let is_static jfield = FlagField.is_set jfield.access_flags FlagField.Static

let is_final jfield = FlagField.is_set jfield.access_flags FlagField.Final

let is_protected jfield = FlagField.is_set jfield.access_flags FlagField.Protected

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

let constant_value jfield =
  let attrs = List.filter_map jfield.attrs ~f:(fun attr ->
      match attr with
      | AttrField.ConstantValue index -> Some index
      | _ -> None
    )
  in
  match List.length attrs  with
  | 1 -> Some (List.hd_exn attrs)
  | _ -> None

let descriptor jfield = jfield.mid.MemberID.descriptor

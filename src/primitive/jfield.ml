open Core
open Accflag
open VMError
open Attribute
open Types

include Types.InnField

let create jclass mid access_flags attrs =
  { jclass; mid; access_flags; attrs }

let jclass jfield = jfield.jclass

let mid jfield = jfield.mid

let name jfield = MemberID.name jfield.mid

let descriptor jfield = MemberID.descriptor jfield.mid

let access_flags jfield = jfield.access_flags

let attrs jfield = jfield.attrs

let is_final jfield = FlagField.is_set jfield.access_flags FlagField.Final

let is_static jfield = FlagField.is_set jfield.access_flags FlagField.Static

let is_public jfield = FlagField.is_set jfield.access_flags FlagField.Public

let is_protected jfield = FlagField.is_set jfield.access_flags FlagField.Protected

let is_private jfield = FlagField.is_set jfield.access_flags FlagField.Private

let default_value mid =
  match Descriptor.type_of_field mid.MemberID.descriptor with
  | Descriptor.Byte -> InnValue.Byte 0
  | Descriptor.Short -> InnValue.Short 0
  | Descriptor.Char -> InnValue.Char 0
  | Descriptor.Int -> InnValue.Int Int32.zero
  | Descriptor.Float -> InnValue.Float Float32.zero
  | Descriptor.Long -> InnValue.Long Int64.zero
  | Descriptor.Double -> InnValue.Double 0.
  | Descriptor.Boolean -> InnValue.Boolean 0
  | Descriptor.Class _ -> InnValue.Null

let get_static_value jfield =
  let static_fields = jfield.jclass.InnClass.static_fields in
  if is_static jfield then
    Hashtbl.find_exn static_fields jfield.mid
  else
    raise IncompatibleClassChangeError

let set_static_value jfield value =
  let static_fields = jfield.jclass.InnClass.static_fields in
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

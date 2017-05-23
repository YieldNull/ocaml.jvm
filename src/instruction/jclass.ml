open Core.Std
open Accflag

include Classloader.InnClass

let is_array jclass = Array.is_empty jclass.conspool

let conspool jclass = jclass.conspool

let rec is_member_of_supper jclass mid =
  match jclass.super_class with
  | None -> false
  | Some super ->
    match Hashtbl.find super.fields mid with
    | Some _ -> true
    | _ -> is_member_of_supper super mid

let equal jclass1 jclass2 =
  package_rt_equal jclass1 jclass2 && jclass1.name = jclass2.name

let super_class t = t.super_class

open Core
open Accflag
open Types

include Types.InnClass

let create_array loader name access_flags =
  { name; access_flags;
    super_class = Jloader.find_class loader "java/lang/Object";
    interfaces = []; fields = MemberID.hashtbl ();
    static_methods = MemberID.hashtbl ();
    special_methods = MemberID.hashtbl ();
    virtual_methods = MemberID.hashtbl ();
    clinit_method = None;
    attributes = []; conspool = [||]; loader;
    static_fields = MemberID.hashtbl ();
    initialize_state = Initialized;
    vtable = [||];
    itables = Hashtbl.create ~hashable:String.hashable ();
  }

let name jclass = jclass.name

let access_flags jclass = jclass.access_flags

let super_class jclass = jclass.super_class

let interfaces jclass = jclass.interfaces

let fields jclass = jclass.fields

let static_methods jclass = jclass.static_methods

let special_methods jclass = jclass.special_methods

let virtual_methods jclass = jclass.virtual_methods

let conspool jclass = jclass.conspool

let attributes jclass = jclass.attributes

let loader jclass = jclass.loader

let static_fields jclass = jclass.static_fields

let vtable jclass = jclass.vtable

let itables jclass = jclass.itables

let is_initialized jclass =
  match jclass.initialize_state with
  | Initialized -> true
  | _ -> false

let is_initialing jclass =
  match jclass.initialize_state with
  | Initialing -> true
  | _ -> false

let is_uninitialized jclass =
  match jclass.initialize_state with
  | Uninitialized -> true
  | _ -> false

let set_initialized jclass = jclass.initialize_state <- Initialized

let set_initializing jclass = jclass.initialize_state <- Initialing

let set_uninitialized jclass = jclass.initialize_state <- Uninitialized

let is_array jclass = Array.is_empty jclass.conspool

let is_interface jclass =
  FlagClass.is_set jclass.access_flags FlagClass.Interface

let rec is_subclass ~sub ~super =
  sub.name = super.name ||
  match sub.super_class with
  | Some cls -> is_subclass ~sub:cls ~super:super
  | _ -> false

let is_abstract jclass =
  FlagClass.is_set jclass.access_flags FlagClass.Abstract

let is_public jclass =
  FlagClass.is_set jclass.access_flags FlagClass.Public

let package_name name =
  let cls = Descriptor.component_of_class name in
  let slash = String.rfindi cls ~f:(fun _ c -> c = '/') in
  match slash with
  | Some i -> String.sub cls ~pos:0 ~len:i
  | _ -> "."

let package_rt_equal class1 class2 =
  class1.loader.InnLoader.name = class2.loader.InnLoader.name &&
  package_name class1.name = package_name class2.name

let equal jclass1 jclass2 =
  package_rt_equal jclass1 jclass2 && jclass1.name = jclass2.name

let rec is_member_of_supper jclass mid =
  match jclass.super_class with
  | None -> false
  | Some super ->
    match Hashtbl.find super.fields mid with
    | Some _ -> true
    | _ -> is_member_of_supper super mid

let find_method jclass mid = Hashtbl.find jclass.methods mid

let find_field jclass mid = Hashtbl.find jclass.fields mid

let find_mss_methods jclass mid = []

let find_polymorphic jclass mid =
  if jclass.name = "java/lang/invoke/MethodHandle" then
    let new_mid =
      { MemberID.name = mid.MemberID.name;
        MemberID.descriptor = "([Ljava/lang/Object;)Ljava/lang/Object;"
      }
    in
    let jmethod = Hashtbl.find jclass.methods new_mid in
    match jmethod with
    | Some m -> if FlagMethod.is_set_list m.InnMethod.access_flags
        [FlagMethod.Varargs; FlagMethod.Native] then Some m else None
    | None -> None
  else None

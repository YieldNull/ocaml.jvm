open Core.Std
open Accflag
open Attribute
open Types

type state = InnClass.state =
  | Uninitialized
  | Initialing
  | Initialized

type t = InnClass.t =
  { name : string;
    access_flags  : int;
    super_class   : t option;
    interfaces    : t list;
    fields        : (MemberID.t, InnField.t) Hashtbl.t;
    methods       : (MemberID.t, InnMethod.t) Hashtbl.t;
    conspool      : InnPoolrt.t;
    attributes    : AttrClass.t list;
    loader        : InnLoader.t;
    static_fields : (MemberID.t, InnValue.t) Hashtbl.t;
    mutable initialize_state : InnClass.state;
  }

val create_array : InnLoader.t -> string -> int -> t

val name : t -> string

val access_flags : t -> int

val super_class : t -> t option

val interfaces : t -> t list

val fields : t -> (MemberID.t, InnField.t) Hashtbl.t

val methods : t -> (MemberID.t, InnMethod.t) Hashtbl.t

val conspool : t -> Poolrt.t

val attributes : t -> AttrClass.t list

val loader : t -> InnLoader.t

val static_fields : t -> (MemberID.t, InnValue.t) Hashtbl.t

val is_initialized : t -> bool

val is_initialing : t -> bool

val is_uninitialized : t -> bool

val set_initialized : t -> unit

val set_initializing : t -> unit

val set_uninitialized : t -> unit

val is_array : t -> bool

val is_interface : t -> bool

val is_subclass : sub:t -> super:t -> bool

val is_abstract : t -> bool

val is_public : t -> bool

val equal : t -> t -> bool

val package_name : string -> string

val package_rt_equal : t -> t -> bool

val is_member_of_supper : t -> MemberID.t -> bool

val find_method : t -> MemberID.t -> InnMethod.t option

val find_field : t -> MemberID.t -> InnField.t option

val find_mss_methods : t -> MemberID.t -> InnMethod.t list

val find_polymorphic : t -> MemberID.t -> InnMethod.t option

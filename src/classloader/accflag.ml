open Core.Std

module type S = sig
  type t
  val flag_to_int : t -> int
end

module Make (M : S) = struct
  let is_set integer flag =
    let value = M.flag_to_int flag in
    integer land value = value

  let is_set_list integer flags =
    let value = List.fold flags ~init:0xFFFF
        ~f:(fun acc flag -> acc land (M.flag_to_int flag))
    in
    integer land value = value

  let is_not_set integer flag =
    let value = M.flag_to_int flag in
    integer land value <> value

  let is_not_set_list integer flags =
    List.fold flags ~init:true ~f:(fun acc flag -> acc && is_not_set integer flag)
end

module FlagClass = struct
  module T = struct
    type t =
      | Public | Final | Super | Interface
      | Abstract | Synthetic | Annotation | Enum

    let flag_to_int = function
      | Public     -> 0x0001
      | Final      -> 0x0010
      | Super      -> 0x0020
      | Interface  -> 0x0200
      | Abstract   -> 0x0400
      | Synthetic  -> 0x1000
      | Annotation -> 0x2000
      | Enum       -> 0x4000
  end

  include T
  include Make(T)

  let real_acc integer =
    if is_set integer Public then flag_to_int Public else 0

  let public_flag = flag_to_int Public
end

module FlagField = struct
  module T = struct
    type t =
      | Public | Private | Protected | Static
      | Final | Volatile | Transient | Synthetic | Enum

    let flag_to_int = function
      | Public    -> 0x0001
      | Private   -> 0x0002
      | Protected -> 0x0004
      | Static    -> 0x0008
      | Final     -> 0x0010
      | Volatile  -> 0x0040
      | Transient -> 0x0080
      | Synthetic -> 0x1000
      | Enum      -> 0x4000
  end

  include T
  include Make(T)

end

module FlagMethod = struct
  module T = struct
    type t =
      | Public | Private | Protected | Static
      | Final | Synchronized | Bridge | Varargs
      | Native | Abstract | Strict | Synthetic

    let flag_to_int = function
      | Public       -> 0x0001
      | Private      -> 0x0002
      | Protected    -> 0x0004
      | Static       -> 0x0008
      | Final        -> 0x0010
      | Synchronized -> 0x0020
      | Bridge       -> 0x0040
      | Varargs      -> 0x0080
      | Native       -> 0x0100
      | Abstract     -> 0x0400
      | Strict       -> 0x0800
      | Synthetic    -> 0x1000
  end

  include T
  include Make(T)
end

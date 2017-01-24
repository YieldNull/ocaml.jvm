open Core.Std

type t =
  | Public | Private   | Protected | Static
  | Final  | Super     | Volatile  | Transient
  | Native | Interface | Abstract  | Strict
  | Synthetic | Annotation | Enum

let parse flags =
  let int_to_access_flag = function
    | 0x0001 -> Some Public
    | 0x0002 -> Some Private
    | 0x0004 -> Some Protected
    | 0x0008 -> Some Static
    | 0x0010 -> Some Final
    | 0x0020 -> Some Super
    | 0x0040 -> Some Volatile
    | 0x0080 -> Some Transient
    | 0x0100 -> Some Native
    | 0x0200 -> Some Interface
    | 0x0400 -> Some Abstract
    | 0x0800 -> Some Strict
    | 0x1000 -> Some Synthetic
    | 0x2000 -> Some Annotation
    | 0x4000 -> Some Enum
    | _ -> None
  in
  let base = 0x4000 in
  List.filter_map (List.init 15 ~f:(fun i ->
      let flag = flags land (base lsr i) in
      int_to_access_flag flag
    )) ~f:(fun x -> x)

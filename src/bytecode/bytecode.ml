open Core.Std
open BatIO
open BatIO.BigEndian
open Fmt_error

module ConsPool = Cons_pool

module AccessFlag = struct
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
end

module Field = struct
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrField.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrField.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

module Method = struct
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrMethod.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrMethod.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

type class_file =
  { minor_version : int;
    major_version : int;
    constant_pool : ConsPool.t;
    access_flags  : AccessFlag.t list;
    this_class    : int;
    super_class   : int;
    interfaces    : int list;
    fields        : Field.t list;
    methods       : Method.t list;
    attributes    : Attribute.AttrClass.t list;
  }

let parse input =
  let check_magic input =
    if not (read_ui16 input = 0xCAFE && read_ui16 input = 0xBABE) then
      raise (Class_format_error "Invalid magic")
  in
  let check_end input =
    try
      let _ = read input in
      raise (Class_format_error "Invalid file ending")
    with
    | No_more_input -> ()
  in
  check_magic input;
  let minor_version = read_i16 input in
  let major_version = read_i16 input in
  let pool = ConsPool.create input in
  let access_flags = AccessFlag.parse (read_ui16 input) in
  let this_class = read_ui16 input in
  let super_class = read_ui16 input in
  let interfaces = List.init (read_ui16 input) ~f:(fun _ -> read_ui16 input) in
  let fields = List.init (read_ui16 input) ~f:(fun _ -> Field.parse input pool) in
  let methods = List.init (read_ui16 input) ~f:(fun _ -> Method.parse input pool) in
  let attributes = List.init (read_ui16 input) ~f:(fun _ ->
      Attribute.AttrClass.parse input pool) in
  check_end input;
  { minor_version; major_version; constant_pool = pool;
    access_flags; this_class; super_class;
    interfaces; fields; methods ; attributes
  }

open Core.Std
open Fmt_error

module Array  = Core.Std.Array
module ByteIO = BatIO.BigEndian
module ConsPool = Cons_pool

type access_flag =
  | Public | Private   | Protected | Static
  | Final  | Super     | Volatile  | Transient
  | Native | Interface | Abstract  | Strict
  | Synthetic | Annotation | Enum

type attribute_info =
  { name_index : int;
    length     : int;
  }

type member_info =
  { name          : string;
    access_flags  : access_flag list;
    descriptor    : int;
    attributes    : attribute_info list;
  }

type class_file =
  { minor_version : int;
    major_version : int;
    constant_pool : ConsPool.t;
    access_flags  : access_flag list;
    this_class    : string;
    super_class   : string option;
    interfaces    : string list;
    fileds        : member_info list;
    methods       : member_info list;
    attributes    : attribute_info list;
  }

let int_to_access_flag = function
  | 0x0001 -> Public
  | 0x0002 -> Private
  | 0x0004 -> Protected
  | 0x0008 -> Static
  | 0x0010 -> Final
  | 0x0020 -> Super
  | 0x0040 -> Volatile
  | 0x0080 -> Transient
  | 0x0100 -> Native
  | 0x0200 -> Interface
  | 0x0400 -> Abstract
  | 0x0800 -> Strict
  | 0x1000 -> Synthetic
  | 0x2000 -> Annotation
  | 0x4000 -> Enum
  | _ -> raise (Class_format_error "Invalid access_flag")

let parse path =
  let input = BatFile.open_in path in
  let magic = BatIO.BigEndian.read_real_i32 input in
  let minor_version = BatIO.BigEndian.read_i16 input in
  let major_version = BatIO.BigEndian.read_i16 input in
  let pool = ConsPool.create input in
  printf "%lx\n" magic;
  printf "%d\n" minor_version;
  printf "%d\n" major_version;
  printf "%d\n" (Array.length pool);
  match ConsPool.get pool 11 with
  | ConsPool.Utf8 str -> printf "%s\n" str
  | _ -> assert false

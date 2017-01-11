open Fmt_error

module ByteIO = BatIO.BigEndian
module Array = Core.Std.Array

type member_ref =
  { cls        : string;
    name       : string;
    descriptor : string;
  }

type method_handle =
  { ref_kind  : int;
    ref_index : int;
  }

type name_and_type =
  { name : string;
    descriptor : string;
  }

type invoke_dynamic =
  { attr_index : int;
    name: string;
    descriptor: string;
  }

type elt =
  | Utf8                of string
  | Integer             of int32
  | Float               of float
  | Long                of int64
  | Double              of float
  | Class               of int (* utf8_index *)
  | String              of int (* utf8_index *)
  | Fieldref            of int * int (* class_index, name_and_type_index *)
  | Methodref           of int * int (* class_index, name_and_type_index *)
  | InterfaceMethodref  of int * int (* class_index, name_and_type_index *)
  | NameAndType         of int * int (* name_index, descriptor_index *)
  | MethodHandle        of int * int (* reference_kind, reference_index *)
  | MethodType          of int (* descriptor_index *)
  | InvokeDynamic       of int * int (* bootstrap_method_attr_index, name_and_type_index *)
  | Empty

type t = elt array

exception Element_not_found
exception Invalid_type

let parse input = function
  | 1  -> let len = ByteIO.read_ui16 input in Utf8 (BatIO.nread input len)
  | 3  -> Integer (ByteIO.read_real_i32 input)
  | 4  -> Float (ByteIO.read_float input)
  | 5  -> Long (ByteIO.read_i64 input)
  | 6  -> Double (ByteIO.read_double input)
  | 7  -> Class (ByteIO.read_ui16 input)
  | 8  -> String (ByteIO.read_ui16 input)
  | 9  -> Fieldref (ByteIO.read_ui16 input, ByteIO.read_ui16 input)
  | 10 -> Methodref (ByteIO.read_ui16 input, ByteIO.read_ui16 input)
  | 11 -> InterfaceMethodref (ByteIO.read_ui16 input, ByteIO.read_ui16 input)
  | 12 -> NameAndType (ByteIO.read_ui16 input, ByteIO.read_ui16 input)
  | 15 -> MethodHandle (BatIO.read_byte input, ByteIO.read_ui16 input)
  | 16 -> MethodType (ByteIO.read_ui16 input)
  | 18 -> InvokeDynamic (ByteIO.read_ui16 input, ByteIO.read_ui16 input)
  | _  -> raise (Class_format_error "Invalid Constant Pool Flag")


let init size = Array.create ~len:size Empty

let create input =
  let pool = init (ByteIO.read_ui16 input) in
  for i = 1 to Array.length pool - 1 do
    let tag = BatIO.read_byte input in
    pool.(i) <- parse input tag
  done;
  pool

let get pool index =
  if index < Array.length pool then
    pool.(index)
  else
    raise Element_not_found

let get_utf8 pool index =
  match get pool index with
  | Utf8 str -> str
  | _ -> raise Invalid_type

let get_integer pool index =
  match get pool index with
  | Integer i -> i
  | _ -> raise Invalid_type

let get_float pool index =
  match get pool index with
  | Float f -> f
  | _ -> raise Invalid_type

let get_long pool index =
  match get pool index with
  | Long l -> l
  | _ -> raise Invalid_type

let get_double pool index =
  match get pool index with
  | Double d -> d
  | _ -> raise Invalid_type

let get_class pool index =
  match get pool index with
  | Class index -> get_utf8 pool index
  | _ -> raise Invalid_type

let get_string pool index =
  match get pool index with
  | String index -> get_utf8 pool index
  | _ -> raise Invalid_type

let get_name_and_type pool index =
  match get pool index with
  | NameAndType (name_index, descriptor_index) ->
    let name = get_utf8 pool name_index in
    let descriptor = get_utf8 pool descriptor_index in
    { name; descriptor }
  | _ -> raise Invalid_type

let get_memberref pool index =
  match get pool index with
  | Fieldref (class_index, name_and_type_index)
  | Methodref (class_index, name_and_type_index)
  | InterfaceMethodref (class_index, name_and_type_index) ->
    let nt= get_name_and_type pool name_and_type_index in
    { cls = get_class pool class_index;
      name = nt.name;
      descriptor = nt.descriptor;
    }
  | _ -> raise Invalid_type

let get_method_handle pool index =
  match get pool index with
  | MethodHandle (ref_kind, ref_index)-> { ref_kind; ref_index }
  | _ -> raise Invalid_type

let get_method_type pool index =
  match get pool index with
  | MethodType index -> get_utf8 pool index
  | _ -> raise Invalid_type

let get_invoke_dynamic pool index =
  match get pool index with
  | InvokeDynamic (bootstrap_method_attr_index, name_and_type_index) ->
    let nt = get_name_and_type pool name_and_type_index in
    { attr_index = bootstrap_method_attr_index;
      name = nt.name;
      descriptor = nt.descriptor;
    }
  | _ -> raise Invalid_type

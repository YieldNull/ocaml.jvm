open VMError
open Core.Std
open BatIO
open BatIO.BigEndian

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
  | Byte8Placeholder

type t = elt array

let parse input = function
  | 1  -> let len = read_ui16 input in Utf8 (nread input len)
  | 3  -> Integer (read_real_i32 input)
  | 4  -> Float (read_float input)
  | 5  -> Long (read_i64 input)
  | 6  -> Double (read_double input)
  | 7  -> Class (read_ui16 input)
  | 8  -> String (read_ui16 input)
  | 9  -> Fieldref (read_ui16 input, read_ui16 input)
  | 10 -> Methodref (read_ui16 input, read_ui16 input)
  | 11 -> InterfaceMethodref (read_ui16 input, read_ui16 input)
  | 12 -> NameAndType (read_ui16 input, read_ui16 input)
  | 15 -> MethodHandle (read_byte input, read_ui16 input)
  | 16 -> MethodType (read_ui16 input)
  | 18 -> InvokeDynamic (read_ui16 input, read_ui16 input)
  | i  -> raise (Class_format_error ("Invalid Constant Pool Flag " ^ string_of_int i))

let create input =
  let size = read_ui16 input in
  let is_8_bytes = ref false in
  Array.init (size - 1) ~f:(fun _ ->
      if !is_8_bytes then begin
        is_8_bytes := false;
        Byte8Placeholder
      end
      else begin
        let tag = read_byte input in
        let entry = parse input tag in
        match entry with
        | Long _ | Double _ -> is_8_bytes := true; entry
        | _ -> entry
      end
    )

let raise_index_error index =
  raise (Class_format_error (sprintf "Invalid constant pool index %d" index))

let get pool index =
  let i = index - 1 in
  if i < Array.length pool then
    pool.(i)
  else
    raise_index_error index

let get_utf8 pool index =
  match get pool index with
  | Utf8 str -> str
  | _ -> raise_index_error index

let get_integer pool index =
  match get pool index with
  | Integer i -> i
  | _ -> raise_index_error index

let get_float pool index =
  match get pool index with
  | Float f -> f
  | _ -> raise_index_error index

let get_long pool index =
  match get pool index with
  | Long l -> l
  | _ -> raise_index_error index

let get_double pool index =
  match get pool index with
  | Double d -> d
  | _ -> raise_index_error index

let get_class pool index =
  match get pool index with
  | Class index -> get_utf8 pool index
  | _ -> raise_index_error index

let get_string pool index =
  match get pool index with
  | String index -> get_utf8 pool index
  | _ -> raise_index_error index

let get_name_and_type pool index =
  match get pool index with
  | NameAndType (name_index, descriptor_index) -> name_index, descriptor_index
  | _ -> raise_index_error index

let get_memberref pool index =
  match get pool index with
  | Fieldref (class_index, name_and_type_index)
  | Methodref (class_index, name_and_type_index)
  | InterfaceMethodref (class_index, name_and_type_index) ->
    class_index, name_and_type_index
  | _ -> raise_index_error index

let get_method_handle pool index =
  match get pool index with
  | MethodHandle (ref_kind, ref_index)-> ref_kind, ref_index
  | _ -> raise_index_error index

let get_method_type pool index =
  match get pool index with
  | MethodType index -> get_utf8 pool index
  | _ -> raise_index_error index

let get_invoke_dynamic pool index =
  match get pool index with
  | InvokeDynamic (bootstrap_method_attr_index, name_and_type_index) ->
    bootstrap_method_attr_index, name_and_type_index
  | _ -> raise_index_error index

open Fmt_error

module Array  = Core.Std.Array
module ByteIO = BatIO.BigEndian
module ConsPool = Cons_pool

type t =
  | ConstantValue of constant_value
  | Code of code
  | Placeholder

and constant_value = { value_index : int; }
and code_exn =
  { start_pc   : int;
    end_pc     : int;
    handler_pc : int;
    catch_type : int;
  }
and code =
  { max_stack : int;
    max_locals : int;
    code : int array;
    exn_table : code_exn array;
    attributes : t array;
  }

let assert_len input len =
  let real_len = ByteIO.read_i32 input in
  if len <> real_len then raise (Class_format_error "Invalid length")

let rec parse input ~pool ~count =
  let parse_constant_value input _ =
    assert_len input 2;
    ConstantValue {value_index = ByteIO.read_ui16 input}
  in

  let parse_code input pool =
    let _ = ByteIO.read_real_i32 input in
    let max_stack = ByteIO.read_ui16 input in
    let max_locals = ByteIO.read_ui16 input in
    let code_len = ByteIO.read_i32 input in
    if code_len <= 0 || code_len >= 65536
    then
      raise (Class_format_error "Invalid Code length")
    else
      let code = Array.create ~len:code_len 0 in
      for i = 0 to code_len - 1 do
        code.(i) <- BatIO.read_byte input
      done;
      let exn_len = ByteIO.read_ui16 input in
      let exn_table = Array.create ~len:exn_len
          { start_pc   = 0;
            end_pc     = 0;
            handler_pc = 0;
            catch_type = 0;
          } in
      for i = 0 to exn_len -1 do
        let start_pc = ByteIO.read_ui16 input in
        let end_pc = ByteIO.read_ui16 input in
        let handler_pc = ByteIO.read_ui16 input in
        let catch_type = ByteIO.read_ui16 input in
        exn_table.(i) <- {start_pc;end_pc;handler_pc;catch_type}
      done;
      let attr_count = ByteIO.read_ui16 input in
      let attributes = parse input ~pool:pool ~count:attr_count in
      Code { max_stack; max_locals; code; exn_table; attributes}
  in

  let name_to_parser = function
    | "ConstantValue" -> Some parse_constant_value
    | "Code" -> Some parse_code
    | _ -> None
  in

  let attributes = Array.create ~len:count Placeholder in
  for i = 0 to count - 1 do
    let parser = name_to_parser (ConsPool.get_utf8 pool (ByteIO.read_ui16 input)) in
    match parser with
    | Some f -> attributes.(i) <- f input pool
    | _ -> ()
  done;
  attributes

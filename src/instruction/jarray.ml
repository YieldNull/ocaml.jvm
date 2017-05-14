open Core.Std
open VMError
open Accflag
open Jvalue

include Classloader.InnObject

let check_bounds arr index =
  if index < 0 || index >= Array.length arr.values
  then raise ArrayIndexOutOfBoundsException

let create_primitive len type_code =
  let len = Int32.to_int_exn len in
  if len < 0 then raise NegativeArraySizeException;
  let default = match type_code with
    | 4 -> Boolean false
    | 5 -> Char 0
    | 6 -> Float Float32.zero
    | 7 -> Double 0.0
    | 8 -> Byte 0
    | 9 -> Short 0
    | 10 -> Int Int32.zero
    | 11 -> Long Int64.zero
    | _ -> raise VirtualMachineError
  in
  Arr { jclass = None; values = Array.create ~len default }

let create_reference jclass len =
  let len = Int32.to_int_exn len in
  if len < 0 then raise NegativeArraySizeException;
  Arr { jclass = Some jclass; values = Array.create ~len (Reference Null)}

let rec create_multiple jclass dimensions lens =
  if dimensions <= 0 then raise VirtualMachineError;
  let len = Int32.to_int_exn (List.hd_exn lens) in
  if len < 0 then raise NegativeArraySizeException;
  if len > 0 then
    let name = jclass.Jclass.name in
    let subname = String.sub name ~pos:1 ~len:(String.length name - 1) in
    let component = if dimensions > 1 then
        let subclass = Classloader.load_class jclass.Jclass.loader subname in
        Reference (create_multiple subclass (dimensions - 1) (List.tl_exn lens))
      else
        Jfield.default_value {MemberID.name = ""; MemberID.descriptor = subname}
    in
    Arr { jclass = Some jclass; values = Array.create ~len component }
  else
    Arr { jclass = Some jclass; values = Array.empty () }

let get_arr_exn objref =
  match objref with
  | Arr arr -> arr
  | Null -> raise NullPointerException
  | _ -> raise VirtualMachineError

let length objref =
  let arr = get_arr_exn objref in
  Int32.of_int_exn (Array.length arr.values)

let load objref i =
  let index = Int32.to_int_exn i in (* TODO catch exception *)
  let arr = get_arr_exn objref in
  check_bounds arr index;
  arr.values.(index)

let store objref i value =
  let index = Int32.to_int_exn i in (* TODO catch exception *)
  let arr = get_arr_exn objref in
  check_bounds arr index;
  arr.values.(index) <- value

let get_values arrref = arrref.values

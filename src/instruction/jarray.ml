open Core
open VMError
open Accflag
open Jvalue

let equal arr1 arr2 = phys_equal arr1 arr2

let check_bounds arr index =
  if index < 0 || index >= Array.length arr.values
  then raise ArrayIndexOutOfBoundsException

let create_primitive jclass len type_code =
  let len = Int32.to_int_exn len in
  if len < 0 then raise NegativeArraySizeException;
  let default = match type_code with
    | 4 -> Boolean 0
    | 5 -> Char 0
    | 6 -> Float Float32.zero
    | 7 -> Double 0.0
    | 8 -> Byte 0
    | 9 -> Short 0
    | 10 -> Int 0l
    | 11 -> Long 0L
    | _ -> raise VirtualMachineError
  in
  { jclass = Classloader.root_class (); values = Array.create ~len default }

let create_reference jclass len =
  let len = Int32.to_int_exn len in
  if len < 0 then raise NegativeArraySizeException;
  { jclass; values = Array.create ~len Null }

let rec create_multiple jclass dimensions lens =
  if dimensions <= 0 then raise VirtualMachineError;
  let len = Int32.to_int_exn (List.hd_exn lens) in
  if len < 0 then raise NegativeArraySizeException;
  if len > 0 then
    let name = Jclass.name jclass in
    let subname = String.sub name ~pos:1 ~len:(String.length name - 1) in
    let component = if dimensions > 1 then
        let subclass = Classloader.load_class (Jclass.loader jclass) subname in
        Array (create_multiple subclass (dimensions - 1) (List.tl_exn lens))
      else
        Jfield.default_value {MemberID.name = ""; MemberID.descriptor = subname}
    in
    { jclass; values = Array.create ~len component }
  else
    { jclass; values = [||] }

let length arr =
  Int32.of_int_exn (Array.length arr.values)

let load arr i =
  let index = Int32.to_int_exn i in (* TODO catch exception *)
  check_bounds arr index;
  arr.values.(index)

let store arr i value =
  let index = Int32.to_int_exn i in (* TODO catch exception *)
  check_bounds arr index;
  arr.values.(index) <- value

let get_values arr = arr.values

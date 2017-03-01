open Core.Std
open Float32
open VMError
open Accflag
open Jvalue

include Classloader.InnObject

let check_bounds arr index =
  if index < 0 || index >= Array.length arr.values
  then raise ArrayIndexOutOfBoundsException

let create_obj jclass =
  if FlagClass.is_set jclass.Jclass.access_flags FlagClass.Abstract
  then raise InstantiationError;
  let fields = Hashtbl.create ~hashable:MemberID.hashable () in
  Hashtbl.iteri jclass.Jclass.fields ~f:(fun ~key:memid ~data:field ->
      if Jfield.is_static field then
        Hashtbl.add_exn fields ~key:memid ~data:(Jfield.default_value memid)
    );
  Obj { jclass; fields }

let create_arr len type_code =
  let len = Int32.to_int_exn len in
  if len < 0 then raise NegativeArraySizeException;
  let default = match type_code with
    | 4 -> Boolean false
    | 5 -> Char 0
    | 6 -> Float 0.0
    | 7 -> Double 0.0
    | 8 -> Byte 0
    | 9 -> Short 0
    | 10 -> Int Int32.zero
    | 11 -> Long Int64.zero
    | _ -> raise VirtualMachineError
  in
  Arr { jclass = None; values = Array.create ~len default }

let create_ref_arr jclass len =
  let len = Int32.to_int_exn len in
  if len < 0 then raise NegativeArraySizeException;
  Arr { jclass = Some jclass; values = Array.create ~len (Reference Null)}

let rec create_multi_arr jclass dimensions lens =
  if dimensions <= 0 then raise VirtualMachineError;
  let len = Int32.to_int_exn (List.hd_exn lens) in
  if len < 0 then raise NegativeArraySizeException;
  if len > 0 then
    let name = jclass.Jclass.name in
    let subname = String.sub name ~pos:1 ~len:(String.length name - 1) in
    let component = if dimensions > 1 then
        let subclass = Classloader.load_class jclass.Jclass.loader subname in
        Reference (create_multi_arr subclass (dimensions - 1) (List.tl_exn lens))
      else
        Jfield.default_value {MemberID.name = ""; MemberID.descriptor = subname}
    in
    Arr { jclass = Some jclass; values = Array.create ~len component }
  else
    Arr { jclass = Some jclass; values = Array.empty () }

let get_arr_exn obj =
  match obj with
  | Arr arr -> arr
  | Null -> raise NullPointerException
  | _ -> raise VirtualMachineError

let get_obj obj =
  match obj with
  | Obj o -> o
  | _ -> raise VirtualMachineError

let get_array_length obj =
  let arr = get_arr_exn obj in
  Int32.of_int_exn (Array.length arr.values)

let load obj i =
  let index = Int32.to_int_exn i in (* TODO catch exception *)
  let arr = get_arr_exn obj in
  check_bounds arr index;
  arr.values.(index)

let store obj i value =
  let index = Int32.to_int_exn i in (* TODO catch exception *)
  let arr = get_arr_exn obj in
  check_bounds arr index;
  arr.values.(index) <- value

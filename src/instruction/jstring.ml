open Core.Std
open VMError

let pool = Hashtbl.create ~hashable:String.hashable ()


(* create a String object and init field "char[] value" in java.lang.String*)
let create str uchars =
  let strobj = Jobject.create @@ Classloader.load_class Classloader.bootstrap_loader "java.lang.String" in
  let value_memid = { MemberID.name = "value"; MemberID.descriptor = "[C" } in
  let arr = { Jobject.jclass = None; Jobject.values = uchars } in
  Jobject.set_field_value_exn strobj value_memid (Jvalue.Reference (Jobject.Arr arr));
  Hashtbl.add_exn pool ~key:str ~data:strobj;
  strobj

let of_string str =
  let uchars = Unicode.modified_utf8_to_uchar_arr str in
  create str uchars

let of_uchars uchars =
  let str = Unicode.uchar_arr_to_modified_utf8 uchars in
  create str uchars

let find str = Hashtbl.find pool str

let find_or_create str =
  match find str with
  | Some obj -> obj
  | _ -> of_string str

let intern strobj =
  let value_memid = { MemberID.name = "value"; MemberID.descriptor = "[C" } in
  let value = Jobject.get_field_value_exn strobj value_memid in
  match value with
  | Jvalue.Reference (Jobject.Arr arr) ->
    begin
      let uchars = arr.Jobject.values in
      let str = Unicode.uchar_arr_to_modified_utf8 uchars in
      match find str with
      | Some obj -> obj
      | _ -> create str uchars
    end
  | _ -> assert false

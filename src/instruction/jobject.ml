open Core.Std
open VMError
open Accflag
open Jvalue

include Classloader.InnObject

let create jclass =
  if FlagClass.is_set jclass.Jclass.access_flags FlagClass.Abstract
  then raise InstantiationError;
  let fields = Hashtbl.create ~hashable:MemberID.hashable () in
  Hashtbl.iteri jclass.Jclass.fields ~f:(fun ~key:memid ~data:field ->
      if Jfield.is_static field then
        Hashtbl.add_exn fields ~key:memid ~data:(Jfield.default_value memid)
    );
  Obj { jclass; fields }


let get_obj_exn jobject =
  match jobject with
  | Obj obj -> obj
  | Null -> raise NullPointerException
  | _ -> raise VirtualMachineError

let get_field_value jobject jfield =
  let obj = get_obj_exn jobject in
  Hashtbl.find_exn obj.fields jfield.Jfield.mid

let set_field_value jobject jfield value =
  let obj = get_obj_exn jobject in
  Hashtbl.set obj.fields ~key:jfield.Jfield.mid ~data:value

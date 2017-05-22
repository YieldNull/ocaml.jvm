open Core.Std
open VMError
open Accflag
open Jvalue

let create jclass =
  if FlagClass.is_set jclass.Jclass.access_flags FlagClass.Abstract
  then raise InstantiationError;
  let fields = Hashtbl.create ~hashable:MemberID.hashable () in
  Hashtbl.iteri jclass.Jclass.fields ~f:(fun ~key:memid ~data:field ->
      if Jfield.is_static field then
        Hashtbl.add_exn fields ~key:memid ~data:(Jfield.default_value memid)
    );
  { jclass; fields }

let equal obj1 obj2 = phys_equal obj1 obj2

let jclass (objref:jobject) = objref.jclass

let get_field_value_exn objref memid =
  Hashtbl.find_exn objref.fields memid

let set_field_value_exn objref memid value =
  Hashtbl.set objref.fields ~key:memid ~data:value

let get_jfield (objref:jobject) memid =
  Hashtbl.find objref.jclass.Jclass.fields memid

open Core.Std
open VMError

let pool = Hashtbl.create ~hashable:String.hashable


let create str =
  let strobj = Jobject.create @@ Classloader.load_class Classloader.bootstrap_loader "java.lang.String" in
  (* Jobject.set_field_value strobj (Jvalue.Reference (Jobject.Arr str))  *)
  ()

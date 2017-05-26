open Core.Std
open Types

include Types.InnLoader

let bootstrap_loader =
  { name = "_bootstrap";
    classes = Hashtbl.create ~hashable:String.hashable ()
  }

let classes loader = loader.classes

let name loader = loader.name

let equal l1 l2 = l1.name = l2.name

let find_class loader binary_name = Hashtbl.find loader.classes binary_name
let find_class_exn loader binary_name = Hashtbl.find_exn loader.classes binary_name

let is_loader loader binary_name = Option.is_some @@ find_class loader binary_name

let add_class loader jclass =
  ignore @@ Hashtbl.add loader.classes ~key:jclass.InnClass.name ~data:jclass

open VMError
include Classloader.InnPoolrt


let get_class poolrt index =
  match poolrt.(index) with
  | Class x -> x
  | _ -> raise VirtualMachineError

let get_field poolrt index =
  match poolrt.(index) with
  | Fieldref x -> x
  | _ -> raise VirtualMachineError

let get_method poolrt index =
  match poolrt.(index) with
  | Methodref x -> x
  | _ -> raise VirtualMachineError

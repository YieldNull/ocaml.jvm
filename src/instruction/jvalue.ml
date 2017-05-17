open VMError

include Classloader.InnValue

let get_byte value =
  match value with
  | Byte x -> x
  | _ -> raise VirtualMachineError

let get_char value =
  match value with
  | Char x -> x
  | _ -> raise VirtualMachineError

let get_short value =
  match value with
  | Short x -> x
  | _ -> raise VirtualMachineError

let get_int value =
  match value with
  | Int x -> x
  | _ -> raise VirtualMachineError

let get_long value =
  match value with
  | Long x -> x
  | _ -> raise VirtualMachineError

let get_float value =
  match value with
  | Float x -> x
  | _ -> raise VirtualMachineError

let get_double value=
  match value with
  | Double x -> x
  | _ -> raise VirtualMachineError

let get_reference value =
  match value with
  | (Object _ | Array _ | Null) as x -> x
  | _ -> raise VirtualMachineError

let get_reference_or_return_address value =
  match value with
  | (Object _ | Array _ | Null | ReturnAddress) as x -> x
  | _ -> raise VirtualMachineError

let get_object value =
  match value with
  | Object x -> x
  | _ -> raise VirtualMachineError

let get_array value =
  match value with
  | Array x -> x
  | _ -> raise VirtualMachineError

let must_be_byte value =
  match value with
  | Byte _ -> ()
  | _ -> raise VirtualMachineError

let must_be_char value =
  match value with
  | Char _ -> ()
  | _ -> raise VirtualMachineError

let must_be_short value =
  match value with
  | Short _ -> ()
  | _ -> raise VirtualMachineError

let must_be_int value =
  match value with
  | Int _ -> ()
  | _ -> raise VirtualMachineError

let must_be_long value =
  match value with
  | Long _ -> ()
  | _ -> raise VirtualMachineError

let must_be_float value =
  match value with
  | Float _ -> ()
  | _ -> raise VirtualMachineError

let must_be_double value =
  match value with
  | Double _ -> ()
  | _ -> raise VirtualMachineError

let must_be_reference value =
  match value with
  | (Object _ | Array _ | Null) -> ()
  | _ -> raise VirtualMachineError

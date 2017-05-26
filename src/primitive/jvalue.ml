open VMError

include Types.InnValue

let get_int value =
  match value with
  | Int x -> x
  | _ -> raise VirtualMachineError

let get_short value = Int32.to_int @@ get_int value

let get_byte value = Int32.to_int @@ get_int value

let get_char value = Int32.to_int @@ get_int value

let get_boolean value = Int32.to_int @@ get_int value

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

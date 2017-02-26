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
  | Reference x -> x
  | _ -> raise VirtualMachineError

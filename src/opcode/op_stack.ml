open Core.Std
open Jvalue
open Frame
open VMError

let op_pop frame =
  let value = Stack.pop_exn frame.opstack in
  match value with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> ()

let op_pop2 frame =
  let value = Stack.pop_exn frame.opstack in
  match value with
  | Long _ | Double _ -> ()
  | _ -> op_pop frame

let op_dup frame =
  let value = Stack.top_exn frame.opstack in
  match value with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> Stack.push frame.opstack value

let op_dup_x1 frame =
  let value1 = Stack.pop_exn frame.opstack in
  match value1 with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> let value2 = Stack.pop_exn frame.opstack in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ -> let _ = Stack.push frame.opstack value1 in
      Stack.push frame.opstack value2;
      Stack.push frame.opstack value1

let op_dup_x2 frame =
  let value1 = Stack.pop_exn frame.opstack in
  match value1 with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> let value2 = Stack.pop_exn frame.opstack in
    match value2 with
    | Long _ | Double _ ->
      Stack.push frame.opstack value1;
      Stack.push frame.opstack value2;
      Stack.push frame.opstack value1;
    | _ -> let value3 = Stack.pop_exn frame.opstack in
      match value3 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ ->
        Stack.push frame.opstack value1;
        Stack.push frame.opstack value3;
        Stack.push frame.opstack value2;
        Stack.push frame.opstack value1

let op_dup2 frame =
  let value1 = Stack.pop_exn frame.opstack in
  match value1 with
  | Long _ | Double _ -> Stack.push frame.opstack value1; Stack.push frame.opstack value1
  | _ -> let value2 = Stack.pop_exn frame.opstack in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ -> Stack.push frame.opstack value2;
      Stack.push frame.opstack value1;
      Stack.push frame.opstack value2;
      Stack.push frame.opstack value1

let op_dup2_x1 frame =
  let value1 = Stack.pop_exn frame.opstack in
  match value1 with
  | Long _ | Double _ -> begin
      let value2 = Stack.pop_exn frame.opstack in
      match value2 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ -> Stack.push frame.opstack value1;
        Stack.push frame.opstack value2;
        Stack.push frame.opstack value1
    end
  | _ -> let value2 = Stack.pop_exn frame.opstack in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ ->
      let value3 = Stack.pop_exn frame.opstack in
      match value3 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ -> Stack.push frame.opstack value2;
        Stack.push frame.opstack value1;
        Stack.push frame.opstack value3;
        Stack.push frame.opstack value2;
        Stack.push frame.opstack value1

let op_dup2_x2 frame =
  let value1 = Stack.pop_exn frame.opstack in
  match value1 with
  | Long _ | Double _ -> begin
      let value2 = Stack.pop_exn frame.opstack in
      match value2 with
      | Long _ | Double _ -> (* Form 4 *)
        Stack.push frame.opstack value1;
        Stack.push frame.opstack value2;
        Stack.push frame.opstack value1
      | _ -> begin (* Form 2 *)
          let value3 = Stack.pop_exn frame.opstack in
          match value3 with
          | Long _ | Double _ -> raise VirtualMachineError
          | _ -> Stack.push frame.opstack value1;
            Stack.push frame.opstack value3;
            Stack.push frame.opstack value2;
            Stack.push frame.opstack value1
        end
    end
  | _ -> begin
      let value2 = Stack.pop_exn frame.opstack in
      match value2 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ -> let value3 = Stack.pop_exn frame.opstack in
        match value3 with
        | Long _ | Double _ -> (* Form 3 *)
          Stack.push frame.opstack value2;
          Stack.push frame.opstack value1;
          Stack.push frame.opstack value3;
          Stack.push frame.opstack value2;
          Stack.push frame.opstack value1
        | _ -> begin (* Form 1 *)
            let value4 = Stack.pop_exn frame.opstack in
            match value4 with
            | Long _ | Double _ -> raise VirtualMachineError
            | _ -> Stack.push frame.opstack value2;
              Stack.push frame.opstack value1;
              Stack.push frame.opstack value4;
              Stack.push frame.opstack value3;
              Stack.push frame.opstack value2;
              Stack.push frame.opstack value1
          end
    end

let op_swap frame =
  let value1 = Stack.pop_exn frame.opstack in
  match value1 with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> let value2 = Stack.pop_exn frame.opstack in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ -> Stack.push frame.opstack value1;
      Stack.push frame.opstack value2

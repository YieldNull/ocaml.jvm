open Core
open Jvalue
open Frame
open VMError

let op_pop frame =
  let value = stack_pop_exn frame in
  match value with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> ()

let op_pop2 frame =
  let value = stack_pop_exn frame in
  match value with
  | Long _ | Double _ -> ()
  | _ -> op_pop frame

let op_dup frame =
  let value = stack_top_exn frame in
  match value with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> stack_push frame value

let op_dup_x1 frame =
  let value1 = stack_pop_exn frame in
  match value1 with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> let value2 = stack_pop_exn frame in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ -> let _ = stack_push frame value1 in
      stack_push frame value2;
      stack_push frame value1

let op_dup_x2 frame =
  let value1 = stack_pop_exn frame in
  match value1 with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> let value2 = stack_pop_exn frame in
    match value2 with
    | Long _ | Double _ ->
      stack_push frame value1;
      stack_push frame value2;
      stack_push frame value1;
    | _ -> let value3 = stack_pop_exn frame in
      match value3 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ ->
        stack_push frame value1;
        stack_push frame value3;
        stack_push frame value2;
        stack_push frame value1

let op_dup2 frame =
  let value1 = stack_pop_exn frame in
  match value1 with
  | Long _ | Double _ -> stack_push frame value1; stack_push frame value1
  | _ -> let value2 = stack_pop_exn frame in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ -> stack_push frame value2;
      stack_push frame value1;
      stack_push frame value2;
      stack_push frame value1

let op_dup2_x1 frame =
  let value1 = stack_pop_exn frame in
  match value1 with
  | Long _ | Double _ -> begin
      let value2 = stack_pop_exn frame in
      match value2 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ -> stack_push frame value1;
        stack_push frame value2;
        stack_push frame value1
    end
  | _ -> let value2 = stack_pop_exn frame in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ ->
      let value3 = stack_pop_exn frame in
      match value3 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ -> stack_push frame value2;
        stack_push frame value1;
        stack_push frame value3;
        stack_push frame value2;
        stack_push frame value1

let op_dup2_x2 frame =
  let value1 = stack_pop_exn frame in
  match value1 with
  | Long _ | Double _ -> begin
      let value2 = stack_pop_exn frame in
      match value2 with
      | Long _ | Double _ -> (* Form 4 *)
        stack_push frame value1;
        stack_push frame value2;
        stack_push frame value1
      | _ -> begin (* Form 2 *)
          let value3 = stack_pop_exn frame in
          match value3 with
          | Long _ | Double _ -> raise VirtualMachineError
          | _ -> stack_push frame value1;
            stack_push frame value3;
            stack_push frame value2;
            stack_push frame value1
        end
    end
  | _ -> begin
      let value2 = stack_pop_exn frame in
      match value2 with
      | Long _ | Double _ -> raise VirtualMachineError
      | _ -> let value3 = stack_pop_exn frame in
        match value3 with
        | Long _ | Double _ -> (* Form 3 *)
          stack_push frame value2;
          stack_push frame value1;
          stack_push frame value3;
          stack_push frame value2;
          stack_push frame value1
        | _ -> begin (* Form 1 *)
            let value4 = stack_pop_exn frame in
            match value4 with
            | Long _ | Double _ -> raise VirtualMachineError
            | _ -> stack_push frame value2;
              stack_push frame value1;
              stack_push frame value4;
              stack_push frame value3;
              stack_push frame value2;
              stack_push frame value1
          end
    end

let op_swap frame =
  let value1 = stack_pop_exn frame in
  match value1 with
  | Long _ | Double _ -> raise VirtualMachineError
  | _ -> let value2 = stack_pop_exn frame in
    match value2 with
    | Long _ | Double _ -> raise VirtualMachineError
    | _ -> stack_push frame value1;
      stack_push frame value2

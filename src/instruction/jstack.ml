open Core.Std
open Opcodes

type t =
  { mutable current_frame : Frame.t;
    frame_stack : (Frame.t) Stack.t;
  }

let create jmethod =
  let current_frame = Frame.create jmethod ~f:(fun _ -> Jvalue.Reference Jobject.Null) in
  let frame_stack = Stack.create () in
  Stack.push frame_stack current_frame;
  { current_frame; frame_stack; }

let handle_return t value =
  let _ = Stack.pop_exn t.frame_stack in
  let former = Stack.pop t.frame_stack in
  match former with
  | Some frame -> begin match value with
      | Some v -> Stack.push frame.Frame.opstack v
      | _ -> ()
    end
  | _ -> ()

let handle_new_frame t frame =
  ()

let run_opcode t frame =
  match Frame.read_byte frame with
  | 0xac -> let value = op_ireturn frame in handle_return t value; value
  | 0xad -> let value = op_lreturn frame in handle_return t value; value
  | 0xae -> let value = op_freturn frame in handle_return t value; value
  | 0xaf -> let value = op_dreturn frame in handle_return t value; value
  | 0xb0 -> let value = op_areturn frame in handle_return t value; value
  | 0xb1 -> let value = op_return frame in handle_return t value; value
  | 0xb6 -> handle_new_frame t @@ op_invokevirtual frame; None
  | 0xb7 -> handle_new_frame t @@ op_invokespecial frame; None
  | 0xb8 -> handle_new_frame t @@ op_invokestatic frame; None
  | 0xb9 -> handle_new_frame t @@ op_invokeinterface frame; None
  | 0xba -> handle_new_frame t @@ op_invokedynamic frame; None
  | x -> let f = opcode_to_func x in f frame; None

let execute t =
  let result = ref None in
  while not (Stack.is_empty t.frame_stack) do
    let frame = Stack.top_exn t.frame_stack in
    while not (Frame.end_of_codes frame) do
      result := run_opcode t frame
    done
  done;
  !result

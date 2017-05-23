open Core.Std
open Opcodes
open Jvalue

type t =
  { mutable current_frame : Frame.t;
    frame_stack : (Frame.t) Stack.t;
    mutable return_value : Jvalue.t option;
  }

exception MethodInvoke

let create jmethod args =
  let current_frame = Frame.create jmethod args in
  let frame_stack = Stack.create () in
  Stack.push frame_stack current_frame;
  { current_frame; frame_stack; return_value = None }

let handle_return t value =
  let current = Stack.pop_exn t.frame_stack in
  Frame.set_end_of_codes current;
  let former = Stack.top t.frame_stack in
  match former with
  | Some frame -> begin
      match value with
      | Some v -> Frame.stack_push frame v
      | _ -> ()
    end
  | _ -> t.return_value <- value; ()

let handle_new_frame t frame =
  Stack.push t.frame_stack frame;
  raise MethodInvoke

let run_opcode t frame =
  match Frame.read_byte frame with
  | 0xac -> handle_return t @@ op_ireturn frame
  | 0xad -> handle_return t @@ op_lreturn frame
  | 0xae -> handle_return t @@ op_freturn frame
  | 0xaf -> handle_return t @@ op_dreturn frame
  | 0xb0 -> handle_return t @@ op_areturn frame
  | 0xb1 -> handle_return t @@ op_return frame
  | 0xb6 -> handle_new_frame t @@ op_invokevirtual frame
  | 0xb7 -> handle_new_frame t @@ op_invokespecial frame
  | 0xb8 -> handle_new_frame t @@ op_invokestatic frame
  (* | 0xb9 -> handle_new_frame t @@ op_invokeinterface frame *)
  (* | 0xba -> handle_new_frame t @@ op_invokedynamic frame *)
  | x -> let f = opcode_to_func x in f frame

let execute t =
  while not (Stack.is_empty t.frame_stack) do
    let frame = Stack.top_exn t.frame_stack in
    try
      while not (Frame.is_end_of_codes frame) do
        run_opcode t frame
      done
    with MethodInvoke -> ()
  done;
  t.return_value

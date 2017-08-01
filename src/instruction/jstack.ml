open Core
open Opcodes
open Jvalue
open VMError

exception MethodInvokeException

type t =
  {
    frame_stack : (Frame.t) Stack.t;
    mutable return_value : Jvalue.t option;
  }

let create jmethod args =
  let frame = Frame.create jmethod args in
  let frame_stack = Stack.create () in
  Stack.push frame_stack frame;
  { frame_stack; return_value = None }

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

let handle_invoke t frame =
  Stack.push t.frame_stack frame;
  raise MethodInvokeException

let rec initialize_class jclass =
  let init_final jclass =
    Hashtbl.iter (Jclass.fields jclass) ~f:(fun jfield ->
        if Jfield.is_final jfield && Jfield.is_static jfield then
          let i = Jfield.constant_value jfield in
          match i with
          | None -> ()
          | Some index ->
            let conspool = Jclass.conspool jclass in
            let value =
              match Descriptor.type_of_field (Jfield.descriptor jfield) with
              | (Descriptor.Byte | Descriptor.Short
                | Descriptor.Char | Descriptor.Boolean
                | Descriptor.Int) -> Int (Poolrt.get_int conspool index)
              | Descriptor.Float -> Float (Poolrt.get_float conspool index)
              | Descriptor.Long -> Long (Poolrt.get_long conspool index)
              | Descriptor.Double -> Double (Poolrt.get_double conspool index)
              | Descriptor.Class c ->
                if c = "java/lang/String" then
                  let str = Poolrt.get_string conspool index in
                  Object (Jstring.find_or_create str)
                else raise VirtualMachineError
            in
            Jfield.set_static_value jfield value
      )
  in
  let execute_clinit jclass =
    match Jclass.find_method jclass MemberID.cinit with
    | None -> ()
    | Some m -> let stack = create m [||] in ignore @@ execute stack
  in
  let init_super jclass =
    begin match Jclass.super_class jclass with
      | Some super -> initialize_class super
      | _ -> ()
    end;
    List.iter (Jclass.interfaces jclass) ~f:(fun inter ->
        if Hashtbl.exists (Jclass.methods inter) ~f:(fun m ->
            not (Jmethod.is_abstract m || Jmethod.is_static m)
          )
        then initialize_class inter
      )
  in
  if Jclass.is_uninitialized jclass then
    begin
      Jclass.set_initializing jclass;
      init_final jclass;
      init_super jclass;
      execute_clinit jclass;
      Jclass.set_initialized jclass;
    end

and run_opcode t frame =
  let initialize_from_class t frame =
    let index = Frame.read_ui16 frame in
    let binary_name = Poolrt.get_class (Frame.conspool frame) index in
    let current = Frame.current_class frame in
    let jclass = Classloader.resolve_class (Frame.current_loader frame)
        ~caller:(Jclass.name current) ~name:binary_name
    in
    initialize_class jclass;
    jclass
  in
  let initialize_from_method t frame =
    let index = Frame.read_ui16 frame in
    let jmethod = get_static_method_exn frame index in
    let jclass = Jmethod.jclass jmethod in
    initialize_class jclass;
    jmethod
  in
  let initialize_from_field t frame =
    let index = Frame.read_ui16 frame in
    let jfield = get_field frame index in
    let jclass = Jfield.jclass jfield in
    initialize_class jclass;
    jfield
  in
  match Frame.read_byte frame with
  | 0xac -> handle_return t @@ op_ireturn frame
  | 0xad -> handle_return t @@ op_lreturn frame
  | 0xae -> handle_return t @@ op_freturn frame
  | 0xaf -> handle_return t @@ op_dreturn frame
  | 0xb0 -> handle_return t @@ op_areturn frame
  | 0xb1 -> handle_return t @@ op_return frame
  | 0xb6 -> handle_invoke t @@ op_invokevirtual frame
  | 0xb7 -> handle_invoke t @@ op_invokespecial frame
  | 0xb9 -> handle_invoke t @@ op_invokeinterface frame
  (* | 0xba -> handle_invoke t @@ op_invokedynamic frame *)
  | 0xb8 -> let jmethod = initialize_from_method t frame in
    handle_invoke t @@ op_invokestatic frame jmethod
  | 0xb2 -> let jfield = initialize_from_field t frame in
    op_getstatic frame jfield
  | 0xb3 -> let jfield = initialize_from_field t frame in
    op_putstatic frame jfield
  | 0xbb -> let jclass = initialize_from_class t frame in
    op_new frame jclass
  | x -> let f = opcode_to_func x in f frame

and execute t =
  while not (Stack.is_empty t.frame_stack) do
    let frame = Stack.top_exn t.frame_stack in
    if Frame.is_native frame then
      ignore @@ Stack.pop_exn t.frame_stack
    else
      try
        while not (Frame.is_end_of_codes frame) do
          run_opcode t frame
        done
      with MethodInvokeException -> ()
  done;
  t.return_value

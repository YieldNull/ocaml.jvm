open Core
open Attribute
open VMError

type t =
  { mutable pc : int;
    codes : char array;
    jmethod : Jmethod.t;
    conspool : Poolrt.t;
    localvars : Jvalue.t array;
    opstack : (Jvalue.t) Stack.t;
    is_native : bool;
  }

let create jmethod args =
  let rec aux = function
    | [] -> raise AbstractMethodError
    | hd :: tail -> match hd with
      | AttrMethod.Code code -> code
      | _ -> aux tail
  in
  if Jmethod.is_abstract jmethod then raise AbstractMethodError;
  let conspool = Jclass.conspool @@ Jmethod.jclass jmethod in
  let opstack = Stack.create () in
  if not (Jmethod.is_native jmethod) then
    let codeattr = aux (Jmethod.attrs jmethod) in
    let codes = codeattr.Code.code in
    let arg_len = Array.length args in
    let localvars = Array.init codeattr.Code.max_locals ~f:(fun i ->
        if i < arg_len then args.(i)
        else Jvalue.Null
      )
    in { pc = 0; codes; jmethod; conspool;
         localvars; opstack; is_native = false; }
  else
    { pc = 0; codes = [||]; jmethod; conspool;
      localvars = [||]; opstack; is_native = true }

let is_native frame = frame.is_native

let stack_push t value = Stack.push t.opstack value

let stack_pop_exn t = Stack.pop_exn t.opstack

let stack_top_exn t = Stack.top_exn t.opstack

let localvar_get t i = t.localvars.(i)

let localvar_set t i v = t.localvars.(i) <- v

let current_class t = Jmethod.jclass t.jmethod

let current_loader t =
  let jclass = current_class t in
  Jclass.loader jclass

let current_method t = t.jmethod

let current_method_name t = Jmethod.name t.jmethod

let conspool t = t.conspool

let read_byte t =
  let value = Char.to_int t.codes.(t.pc) in
  t.pc <- t.pc + 1;
  value

let read_signed_byte t =
  let chr = Char.to_int t.codes.(t.pc) in
  let value = if chr land 0x80 <> 0 then chr - (0xff + 1) else chr in
  t.pc <- t.pc + 1;
  value

let read_ui16 t =
  let byte1 = Char.to_int t.codes.(t.pc) in
  let byte2 = Char.to_int t.codes.(t.pc + 1) in
  let value = (byte1 lsl 8) lor byte2 in
  t.pc <- t.pc + 2;
  value

let read_i16 t =
  let byte1 = Char.to_int t.codes.(t.pc) in
  let byte2 = Char.to_int t.codes.(t.pc + 1) in
  let short = (byte1 lsl 8) lor byte2 in
  let value = if short land 0x8000 <> 0 then short - (0xffff + 1) else short in
  t.pc <- t.pc + 2;
  value

(* ensure The target address must be
   that of an opcode of an instruction
   within the method that contains this if<cond> instruction *)

let set_pc_offset t offset =
  let target = t.pc + offset in
  if target < 0 || target > Array.length t.codes - 1 then
    raise VirtualMachineError
  else
    t.pc <- target

let is_end_of_codes t = t.pc = Array.length t.codes

let set_end_of_codes t = t.pc <- Array.length t.codes

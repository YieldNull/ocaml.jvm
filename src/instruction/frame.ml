open Core.Std
open Attribute

type t =
  { mutable pc : int;
    codeattr : Code.t;
    codes : char array;
    jmethod : Jmethod.t;
    conspool : Poolrt.t;
    localvars : Jvalue.t array;
    opstack : (Jvalue.t) Stack.t;
  }

let create jmethod ~f:localvar_initializer =
  let rec aux = function
    | [] -> failwith ""
    | hd :: tail -> match hd with
      | AttrMethod.Code code -> code
      | _ -> aux tail
  in
  let codeattr = aux jmethod.Jmethod.attrs in
  let codes= codeattr.Code.code in
  let conspool = Jclass.conspool @@ Jmethod.get_class jmethod in
  let localvars = Array.init codeattr.Code.max_locals ~f:localvar_initializer in
  let opstack = Stack.create () in
  { pc = 0; codeattr; codes; jmethod; conspool; localvars; opstack; }


let current_class frame = frame.jmethod.Jmethod.jclass

let current_loader frame =
  let jclass = current_class frame in
  jclass.Jclass.loader

let load_conspool frame index =
  Poolrt.get frame.conspool index

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

let set_pc_offset t offset = t.pc <- t.pc + offset

let end_of_codes t = t.pc = Array.length t.codes

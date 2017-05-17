open Jvalue

let compile file =
  let dir = Filename.get_temp_dir_name () in
  let command = Printf.sprintf "javac -d %s %s" dir file in
  let class_file = (Filename.chop_extension @@ Filename.basename file) ^ ".class" in
  match Unix.system command with
  | Unix.WEXITED code when code = 0 -> Filename.concat dir class_file
  | _ -> failwith ("Compile failed: " ^ file)

let delete file = Sys.remove file

let load_class class_file =
  Config.add_classpath @@ Filename.dirname class_file;
  let class_name = Filename.chop_extension @@ Filename.basename class_file in
  Classloader.load_class Classloader.bootstrap_loader class_name

let run_method jclass name descriptor =
  let open Core.Std in
  let mid = { MemberID.name = name; MemberID.descriptor = descriptor } in
  let jmethod = Hashtbl.find jclass.Jclass.methods mid in
  match jmethod with
  | Some m -> let jstack = Jstack.create m in
    Jstack.execute jstack
  | _ -> failwith ("Cannot resolve method " ^ (MemberID.to_string mid))

let compare_f32 f1 f2 =
  let ff1 = match f1 with
    | Some (Float f) -> f
    | _ -> failwith ""
  in
  let ff2 = match f2 with
    | Some (Float f) -> f
    | _ -> failwith ""
  in
  Float32.equal ff1 ff2

let compare_string s1 s2 =
  let str1 = match s1 with
    | Some (Object obj) -> obj
    | _ -> failwith ""
  in
  let str2 = match s2 with
    | Some (Object obj) -> obj
    | _ -> failwith ""
  in
  Jstring.compare str1 str2 = 0

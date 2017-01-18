open Core.Std

let classpath = "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/lib/rt.jar"

let load_class name =
  let file = Zip.open_in classpath in
  let entry = Zip.find_entry file (name ^ ".class") in
  let content = Zip.read_entry file entry in
  let input = BatIO.input_string content in
  let _ = Bytecode.parse input in
  Zip.close_in file

let () =
  load_class "java/lang/Object"

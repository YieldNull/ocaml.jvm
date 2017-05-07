open Core.Std

let runtime =
  let pwd = Filename.realpath Sys.executable_name
  in FilePath.concat (FilePath.dirname pwd) "lib/rt.jar"

let classpath =
  let lib = Filename.dirname runtime in
  ref [lib]

let add_classpath item = classpath := item :: !classpath

let get_classpath () = !classpath

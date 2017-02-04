open Core.Std

let runtime =
  let pwd = Filename.realpath Sys.executable_name
  in FilePath.concat (FilePath.dirname pwd) "lib/rt.jar"

let classpath = ref [runtime]

let add_classpath item = classpath := !classpath @ [item]

let get_classpath () = !classpath

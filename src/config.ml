open Core.Std

let runtime =
  let pwd = Filename.realpath Sys.executable_name
  in FilePath.concat (FilePath.dirname pwd) "lib/rt.jar"

let classpath = [runtime]

open Core.Std

let runtime =
  let pwd = Filename.realpath Sys.executable_name
  in FilePath.concat (FilePath.dirname pwd) "lib/rt.jar"

let is_jar filename =
  let fname = String.lowercase filename in
  String.is_suffix fname ~suffix:".jar" || String.is_suffix fname ~suffix:".zip"

let expand_wildcard wildcard =
  let dirname = Filename.dirname wildcard in
  Sys.readdir dirname
  |> Array.filter_map ~f:(fun file ->
      let filename = Filename.concat dirname file in
      match Sys.is_file filename with
      | `Yes when is_jar file -> Some filename
      | _ -> None
    )
  |> Array.to_list

let classpath =
  let bootstrap = Filename.dirname runtime in
  let extension = FilePath.concat bootstrap "ext" in
  let predefined = ["."; ]
                   @ expand_wildcard (FilePath.concat bootstrap "*")
                   @ expand_wildcard (FilePath.concat extension "*")
  in
  ref predefined

let add_classpath item = classpath := item :: !classpath

let get_classpath () = !classpath

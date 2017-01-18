open Core.Std
open Bytecode

let () =
  let dir = Sys.argv.(1) in
  let files = Sys.readdir dir in
  Array.iter files ~f:(fun name ->
      let file = dir ^ "/" ^ name in
      if Sys.is_file file = `Yes && Filename.check_suffix file ".class" then
        let class_file = parse file in
        List.iter class_file.methods ~f:(fun m ->
            List.iter (AccessFlag.parse m.access_flags) ~f:(fun flag ->
                let str = match flag with
                  | AccessFlag.Public -> "Public"
                  | AccessFlag.Private -> "Private"
                  | AccessFlag.Protected -> "Protected"
                  | AccessFlag.Static -> "Static"
                  | AccessFlag.Final -> "Final"
                  | AccessFlag.Super -> "Super"
                  | AccessFlag.Volatile -> "Volatile"
                  | AccessFlag.Transient -> "Transient"
                  | AccessFlag.Native -> "Native"
                  | AccessFlag.Interface -> "Interface"
                  | AccessFlag.Abstract -> "Abstract"
                  | AccessFlag.Strict -> "Strict"
                  | AccessFlag.Synthetic -> "Synthetic"
                  | AccessFlag.Annotation -> "Annotation"
                  | AccessFlag.Enum -> "Enum"
                in printf "%s\n      %s\n" (ConsPool.get_utf8 class_file.constant_pool m.name_index) str
              )
          )
    )

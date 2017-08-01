open Core
open Bytecode

(* Given a directory which contains jar files,
   like "/Library/Java/JavaVirtualMachines/", as Sys.argv[1],
   and then parse those Bytecodes
*)

let () =
  let temp_file = Filename.temp_file "jarlist" "txt" in
  let _ = Sys.command @@ sprintf "find %s -name \"*.jar\" > %s" Sys.argv.(1) temp_file in
  let jars = In_channel.read_lines temp_file in
  List.iter jars ~f:(fun jar ->
      let file = Zip.open_in jar in
      let entries = Zip.entries file in
      List.iter entries ~f:(fun entry ->
          if Filename.check_suffix entry.Zip.filename ".class" then
            let content = Zip.read_entry file entry in
            let input = BatIO.input_string content in
            let _ = Bytecode.load_from_stream input in
            BatIO.close_in input
        );
      printf "OK: %s\n" jar;
      Zip.close_in file
    );
  Sys.remove temp_file

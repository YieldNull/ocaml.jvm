open Core
open Classloader

let () =
  Config.add_classpath Sys.argv.(1);
  let loader = Jloader.bootstrap_loader in
  let file = Zip.open_in Sys.argv.(1) in
  let entries = Zip.entries file in
  List.iter entries ~f:(fun entry ->
      if Filename.check_suffix entry.Zip.filename ".class" then
        let binary_name = String.drop_suffix entry.Zip.filename 6 in
        let _ = load_class loader binary_name in ()
    );
  List.iter (Hashtbl.data @@ Jloader.classes loader)
    ~f:(fun cls -> printf "%s\n" (Jclass.name cls));
  Zip.close_in file

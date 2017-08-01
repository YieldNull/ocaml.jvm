open Core
open Yojson

(* Find native methods of a jar file.
   By default, the runtime "rt.jar" will be used.
   Alternative jar file can be passed as Sys.argv[1].
*)

let find entry file =
  let filename = entry.Zip.filename in
  if (not entry.Zip.is_directory) &&
     (Filename.check_suffix entry.Zip.filename ".class")
  then
    let content = Zip.read_entry file entry in
    let input = BatIO.input_string content in
    let bytecode = Bytecode.load_from_stream input in
    let pool = bytecode.Bytecode.constant_pool in
    let natives = List.filter_map bytecode.Bytecode.methods ~f:(fun mth ->
        if Accflag.FlagMethod.is_set mth.Bytecode.Method.access_flags Accflag.FlagMethod.Native
        then
          let name_index = mth.Bytecode.Method.name_index in
          let descriptor_index = mth.Bytecode.Method.descriptor_index in
          let name = Poolbc.get_utf8 pool name_index in
          let descriptor = Poolbc.get_utf8 pool descriptor_index in
          Some (name, descriptor)
        else
          None
      ) in
    if List.length natives = 0 then None
    else Some (filename, natives)
  else None

module Tree = struct
  module T = struct
    type t =
      | Dir of string
      | File of string * ((string * string) list)
    [@@deriving sexp]

    let compare a b =
      let get_name data = begin
        match data with
        | Dir d -> d
        | File (name,_) -> name
      end
      in String.compare (get_name a) (get_name b)
  end
  include T
  include Comparable.Make(T)
end


let () =
  let jar = if Array.length Sys.argv = 2 then Sys.argv.(1) else Config.runtime in
  let file = Zip.open_in jar in
  let entries = Zip.entries file in
  let natives = List.map entries ~f:(fun entry -> find entry file) in
  Zip.close_in file;
  let map = Hashtbl.create ~hashable:String.hashable () in
  let rec parent_dirs filename =
    let dirname = FilePath.dirname filename in
    let basename = FilePath.basename filename in
    match dirname with
    | "" -> []
    | dir -> (dir, Tree.Dir basename) :: (parent_dirs dir)
  in
  let add key data =
    let files = Hashtbl.find map key in
    match files with
    | None -> Hashtbl.add_exn map ~key:key ~data:(Set.of_list [data] ~comparator:Tree.comparator)
    | Some flist -> Hashtbl.set map ~key:key ~data:(Set.add flist data)
  in
  List.iter natives ~f:(fun entry ->
      match entry with
      | None -> ()
      | Some (filename,natives) ->
        let dirname = FilePath.dirname filename in
        let basename = FilePath.basename filename in
        let data = Tree.File (basename, natives) in
        add dirname data;
        List.iter (parent_dirs dirname) ~f:(fun (dir,file) -> add dir file)
    );
  let dirs = List.filter (Hashtbl.keys map) ~f:(fun d -> FilePath.dirname d = "") in
  let rec to_json dir =
    let files = Hashtbl.find_exn map dir in
    let fjson = List.map (Set.to_list files) ~f:(fun file ->
        match file with
        | Tree.Dir d -> to_json (FilePath.concat dir d)
        | Tree.File (name, natives) ->
          name, `Assoc (List.map natives ~f:(fun (m,descriptor) -> m, `String descriptor))
      )
    in FilePath.basename dir, `Assoc fjson
  in
  let json = `Assoc (List.map dirs ~f:to_json) in
  printf "%s\n" (Yojson.pretty_to_string (Yojson.sort json));

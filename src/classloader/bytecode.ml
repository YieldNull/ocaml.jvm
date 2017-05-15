open VMError
open Core.Std
open BatIO
open BatIO.BigEndian

(*
  When a jar file is first loaded, cache all the packages in it.

  When a class file is first loaded,
    load all the related classes in its constant_pool,
    and cache all the loaded classes in the same jar file.

  When searching a class in classpath,
    find the jar files where the class may be defined by its package.
  If a jar file is cached but not in the jar files, just skip unziping it.
*)

module Field = struct
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrField.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrField.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

module Method = struct
  type t =
    { name_index          : int;
      access_flags        : int;
      descriptor_index    : int;
      attributes          : Attribute.AttrMethod.t list;
    }

  let parse input pool =
    let access_flags = read_ui16 input in
    let name_index = read_ui16 input in
    let descriptor_index = read_ui16 input in
    let attributes = List.init (read_ui16 input) ~f:(fun _ ->
        Attribute.AttrMethod.parse input pool) in
    { name_index; access_flags; descriptor_index; attributes}
end

type t =
  { minor_version : int;
    major_version : int;
    constant_pool : Poolbc.t;
    access_flags  : int;
    this_class    : int;
    super_class   : int;
    interfaces    : int list;
    fields        : Field.t list;
    methods       : Method.t list;
    attributes    : Attribute.AttrClass.t list;
  }

let parse input =
  let check_magic input =
    if not (read_ui16 input = 0xCAFE && read_ui16 input = 0xBABE) then
      raise (ClassFormatError "Invalid magic")
  in
  let check_end input =
    try
      let _ = read input in
      raise (ClassFormatError "Invalid file ending")
    with
    | No_more_input -> ()
  in
  check_magic input;
  let minor_version = read_i16 input in
  let major_version = read_i16 input in
  let pool = Poolbc.create input in
  let access_flags = read_ui16 input in
  let this_class = read_ui16 input in
  let super_class = read_ui16 input in
  let interfaces = List.init (read_ui16 input) ~f:(fun _ -> read_ui16 input) in
  let fields = List.init (read_ui16 input) ~f:(fun _ -> Field.parse input pool) in
  let methods = List.init (read_ui16 input) ~f:(fun _ -> Method.parse input pool) in
  let attributes = List.init (read_ui16 input) ~f:(fun _ ->
      Attribute.AttrClass.parse input pool) in
  check_end input;
  { minor_version; major_version; constant_pool = pool;
    access_flags; this_class; super_class;
    interfaces; fields; methods ; attributes
  }

let loaded_bytecodes = Hashtbl.create ~hashable:String.hashable ()

let package_in_jar = Hashtbl.create ~hashable:String.hashable ()

let cached_jar = Hashtbl.create ~hashable:String.hashable ()

let cache_jar filename in_file =
  List.iter (Zip.entries in_file) ~f:(fun entry ->
      if not entry.Zip.is_directory
      && Filename.check_suffix entry.Zip.filename ".class"
      then begin
        let package = Filename.dirname entry.Zip.filename in
        match Hashtbl.find package_in_jar package with
        | Some jarlist when Option.is_some @@ List.find jarlist ~f:((=) filename) -> ()
        | _ -> Hashtbl.add_multi package_in_jar ~key:package ~data:filename
      end
    );
  Hashtbl.add_exn cached_jar ~key:filename ~data:()

let load_from_infile binary_name in_file =
  try
    let entry = Zip.find_entry in_file (binary_name ^ ".class") in
    let content = Zip.read_entry in_file entry in
    let input = BatIO.input_string content in
    Some (parse input)
  with Not_found -> None

let rec load_related_class in_file bytecode =
  Array.iter bytecode.constant_pool ~f:(fun entry ->
      match entry with
      | Poolbc.Class index ->
        begin
          let binary_name = Poolbc.get_utf8 bytecode.constant_pool index in
          if Option.is_none @@ Hashtbl.find loaded_bytecodes binary_name then
            begin
              match load_from_infile binary_name in_file with
              | Some code ->
                ignore @@ Hashtbl.add loaded_bytecodes ~key:binary_name ~data:code;
                load_related_class in_file code;
              | _ -> ()
            end
        end
      | _ -> ()
    )

let load_from_jar binary_name filename =
  let in_file = Zip.open_in filename in
  let code = load_from_infile binary_name in_file in
  if Option.is_none @@ Hashtbl.find cached_jar filename then
    cache_jar filename in_file
  ;
  begin
    match code with
    | Some bytecode -> load_related_class in_file bytecode
    | _ -> ()
  end;
  Zip.close_in in_file;
  code

let load_from_stream input =
  parse input

let load_from_file binary_name file =
  match Sys.file_exists file with
  | `Yes when (Filename.basename file = binary_name ^ ".class") ->
    let input = BatFile.open_in file in
    let bytecode = parse input in
    BatIO.close_in input; Some bytecode
  | _ -> None

(*
  CLASSPATH: bar.zip foo.jar dir dir/*
  http://docs.oracle.com/javase/8/docs/technotes/tools/unix/classpath.html#A1100592
*)

let find binary_name classpath =
  let package = Filename.dirname binary_name in
  let jar_files = Hashtbl.find package_in_jar package in
  let rec aux paths =
    match paths with
    | hd :: tail -> let bytecode = match Sys.is_directory hd with
        | `Yes ->
          let file = Filename.concat hd (binary_name ^ ".class") in
          load_from_file binary_name file
        | `No -> begin match jar_files with
            | Some jarlist
              when Option.is_none @@ List.find jarlist ~f:((=) hd)
                && Option.is_some @@ Hashtbl.find cached_jar hd (* jar is cached but not in jarlist *)
              -> None
            | _ -> load_from_jar binary_name hd
          end
        | _ -> None
      in if Option.is_none bytecode then aux tail else bytecode
    | [] -> None
  in aux classpath

let load binary_name =
  let bc = Hashtbl.find loaded_bytecodes binary_name in
  match bc with
  | Some code -> code
  | _ -> match find binary_name @@ Config.get_classpath () with
    | Some code -> ignore @@ Hashtbl.add loaded_bytecodes ~key:binary_name ~data:code;code
    | None -> raise (ClassNotFoundException binary_name)

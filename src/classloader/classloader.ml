open VMError
open Core.Std

type t =
  { name : string;
    classes : (string, Jclass.t) Hashtbl.t;
    super : t option;
  }

let loaders = Hashtbl.create ~hashable:String.hashable ()

let create name super =
  let loader = { name; classes = Hashtbl.create ~hashable:String.hashable (); super } in
  Hashtbl.add_exn loaders ~key:name ~data:loader;
  loader

let get name = Hashtbl.find_exn ~key:name

let check_acc referer jclass =
  let acc = jclass.Jclass.access_flags in
  if not (List.exists acc ~f:(fun flag -> flag = Accflag.Public)) then
    let pkg_referer = Jclass.package referer in
    let pkg_target = Jclass.package jclass.Jclass.name in
    if pkg_referer <> pkg_target then
      raise IllegalAccessError

let rec load_class ?referer loader binary_name =
  let create_field field pool =
    let open! Bytecode.Field in
    let field_name = Poolbc.get_utf8 pool field.name_index in
    let field_access_flags = field.access_flags in
    let field_descriptor = Poolbc.get_utf8 pool field.descriptor_index in
    let field_attrs = field.attributes in
    let open! Jclass in
    { field_name; field_descriptor; field_access_flags; field_attrs}
  in
  let create_method mth pool =
    let open! Bytecode.Method in
    let method_name = Poolbc.get_utf8 pool mth.name_index in
    let method_access_flags = mth.access_flags in
    let method_descriptor = Poolbc.get_utf8 pool mth.descriptor_index in
    let method_attrs = mth.attributes in
    let open! Jclass in
    { method_name; method_descriptor; method_access_flags; method_attrs}
  in
  let open! Bytecode in
  let bytecode = Bytecode.load binary_name in
  let pool = bytecode.constant_pool in
  let name = Poolbc.get_class pool bytecode.this_class in
  let access_flags = bytecode.access_flags in
  let super_class  = match bytecode.super_class with
    | 0 -> None
    | i -> let jclass = load_class loader (Poolbc.get_class pool i) in
      check_acc name jclass; Some jclass
  in
  let interfaces = List.map bytecode.interfaces ~f:(fun index ->
      let cls = Poolbc.get_class pool index in
      let jclass = load_class loader cls in
      check_acc name jclass; jclass
    )
  in
  let open! Jclass in
  let fields = List.map bytecode.fields ~f:(fun field -> create_field field pool) in
  let methods = List.map bytecode.methods ~f:(fun mth -> create_method mth pool) in
  let conspool = Array.create ~len:(Array.length pool) Byte8Placeholder in
  let attributes = bytecode.attributes in
  { name; access_flags; super_class; interfaces;
    fields; methods; conspool; attributes; loader; }

let resolve_class loader jclass =
  let rec rsl_class ?referer name =
    let jclass = match name with
      | '['::tail -> rsl_class tail
      | 'L'::tail -> load_class loader.name (String.of_char_list tail)
      | cls -> load_class loader.name (String.of_char_list cls)
    in
    match referer with
    | Some name -> check_acc name jclass; jclass
    | _ -> jclass
  in
  let open! Jclass in
  Hashtbl.add loader.classes ~key:jclass.name ~data:jclass

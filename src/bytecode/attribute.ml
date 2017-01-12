open Core.Std
open BatIO
open BatIO.BigEndian
open Fmt_error

module ConsPool = Cons_pool

module StackMapFrame = struct
  type verification_type =
    | Top
    | Integer
    | Float
    | Double
    | Long
    | Null
    | UninitializedThis
    | Object of int (* cpool_index *)
    | Uninitialized of int (* offset *)

  type same_locals_1_stack_item_frame = { stack : verification_type }

  type same_locals_1_stack_item_frame_extende =
    { offset_delta : int;
      stack : verification_type;
    }

  type chop_frame = { offset_delta : int }
  type same_frame_extended = { offset_delta : int }

  type append_frame =
    { offset_delta : int;
      locals : verification_type array
    }

  type full_frame =
    { offset_delta : int;
      locals : verification_type array;
      stacks : verification_type array;
    }

  type t =
    | SameFrame
    | SameLocals1StackItemFrame of same_locals_1_stack_item_frame
    | SameLocals1StackItemFrameExtended of same_locals_1_stack_item_frame_extende
    | ChopFrame of chop_frame
    | SameFrameExtended of same_frame_extended
    | AppendFrame of append_frame
    | FullFrame of full_frame

  let parse_vtype input =
    match read_byte input with
    | 0 -> Top
    | 1 -> Integer
    | 2 -> Float
    | 3 -> Double
    | 4 -> Long
    | 5 -> Null
    | 6 -> UninitializedThis
    | 7 -> Object (read_ui16 input)
    | 8 -> Uninitialized (read_ui16 input)
    | _ -> raise (Class_format_error "Invalid verification_type")

  let parse input =
    match read_byte input with
    | ft when ft >= 0 && ft <= 63 -> SameFrame
    | ft when ft >= 64 && ft <= 127 -> SameLocals1StackItemFrame
                                         { stack = parse_vtype input }
    | ft when ft = 247 -> SameLocals1StackItemFrameExtended
                            { offset_delta = read_ui16 input;
                              stack = parse_vtype input
                            }
    | ft when ft >= 248 && ft <= 250 -> ChopFrame {offset_delta = read_ui16 input}
    | ft when ft = 251 -> SameFrameExtended {offset_delta = read_ui16 input}
    | ft when ft >= 252 && ft <= 254 ->
      let offset_delta = read_ui16 input in
      let len = ft - 251 in
      let locals = Array.init len ~f:(fun _ -> parse_vtype input) in
      AppendFrame { offset_delta; locals }
    | ft when ft = 255 ->
      let offset_delta = read_ui16 input in
      let locals_len = read_ui16 input in
      let locals = Array.init locals_len ~f:(fun _ -> parse_vtype input) in
      let stacks_len = read_ui16 input in
      let stacks = Array.init stacks_len ~f:(fun _ -> parse_vtype input) in
      FullFrame { offset_delta; locals; stacks}
    | _ -> raise (Class_format_error "Invalid frame type")

end

type inner_class =
  { inner_class_index : int;
    outer_class_index : int;
    inner_name_index  : int;
    inner_class_access_flags : int;
  }

type enclosing_method =
  { class_index : int;
    method_index : int;
  }

type line_number =
  { start_pc : int;
    line_number : int;
  }

type local_variable =
  { start_pc : int;
    length   : int;
    name_index : int;
    descriptor_index : int;
    index    : int;
  }

type local_variable_type =
  { start_pc : int;
    length   : int;
    name_index : int;
    signature_index : int;
    index    : int;
  }

type annotation =
  { type_index : int;
    element_value_pairs : element_value_pair list;
  }
and element_value_pair =
  { name_index : int;
    value : element_value;
  }
and element_value =
  | ConstValueIndex of int
  | EnumConstValue of enum_const_value
  | ClassInfoIndex of int
  | AnnotationValue of annotation
  | ArrayValue of element_value list
and enum_const_value =
  { type_name_index : int;
    const_name_index : int;
  }

type type_annotation =
  { target_type : int;
    target_info : target_info;
    target_path : type_path list;
    type_index  : int;
    element_value_pairs : element_value_pair list;
  }
and target_info =
  | TypeParameterTarget of int
  | SupertypeTarget of int
  | TypeParameterBoundTarget of type_argument_target
  | EmptyTarget
  | MethodFormalParameterTarget of int
  | ThrowsTarget of int
  | LocalvarTarget of localvar list
  | CatchTarget of int
  | OffsetTarget of int
  | TypeArgumentTarget of type_argument_target
and localvar =
  { start_pc : int;
    length   : int;
    index    : int;
  }
and type_parameter_bound_target =
  { type_parameter_index : int;
    bound_index : int;
  }
and type_argument_target =
  { offset : int;
    type_argument_index : int;
  }
and type_path =
  { type_path_kind : int;
    type_argument_index : int;
  }



type t =
  | ConstantValue of int (* value_index *)
  | Code of code
  | StackMapFrame of StackMapFrame.t array
  | Exceptions  of int list
  | InnerClass of inner_class list
  | EnclosingMethod of enclosing_method
  | Synthetic
  | Signature of int (* signature_index *)
  | SourceFile of int (* sourcefile_index *)
  | SourceDebugExtension of int list
  | LineNumberTable of line_number list
  | LocalVariableTable of local_variable list
  | LocalVariableTypeTable of local_variable_type list
  | Deprecated
  | RuntimeVisibleAnnotations of annotation list
  | RuntimeInvisibleAnnotations of annotation list
  | RuntimeVisibleParameterAnnotations of annotation list list
  | RuntimeInvisibleParameterAnnotations of annotation list list
  | Unknown
and code =
  { max_stack : int;
    max_locals : int;
    code : int array;
    exn_table : code_exn array;
    attributes : t list;
  }
and code_exn =
  { start_pc   : int;
    end_pc     : int;
    handler_pc : int;
    catch_type : int;
  }



let parse_constant_value input =
  ConstantValue (read_ui16 input)

let parse_stack_map_table input =
  let count = read_ui16 input in
  let entries = Array.init count ~f:(fun _ -> StackMapFrame.parse input) in
  StackMapFrame entries

let parse_exceptions input =
  let count = read_ui16 input in
  let exceptions = List.init count ~f:(fun _ -> read_ui16 input) in
  Exceptions exceptions

let parse_inner_class input =
  let count = read_ui16 input in
  let classes = List.init count ~f:(fun _ ->
      { inner_class_index = read_ui16 input;
        outer_class_index = read_ui16 input;
        inner_name_index  = read_ui16 input;
        inner_class_access_flags = read_ui16 input;
      }
    ) in
  InnerClass classes

let parse_enclosing_method input =
  let class_index = read_ui16 input in
  let method_index = read_ui16 input in
  EnclosingMethod { class_index; method_index }

let parse_line_number_table input =
  let count = read_ui16 input in
  let line_number_table = List.init count ~f:(fun _ ->
      { start_pc = read_ui16 input;
        line_number = read_ui16 input;
      }
    )
  in LineNumberTable line_number_table

let parse_local_variable_table input =
  let count = read_ui16 input in
  let local_variable_table = List.init count ~f:(fun _ ->
      { start_pc = read_ui16 input;
        length = read_ui16 input;
        name_index = read_ui16 input;
        descriptor_index = read_ui16 input;
        index = read_ui16 input;
      }
    )
  in LocalVariableTable local_variable_table

let parse_local_variable_type_table input =
  let count = read_ui16 input in
  let type_table = List.init count ~f:(fun _ ->
      { start_pc = read_ui16 input;
        length = read_ui16 input;
        name_index = read_ui16 input;
        signature_index = read_ui16 input;
        index = read_ui16 input;
      }
    )
  in LocalVariableTypeTable type_table

let rec parse_annotation input =
  let rec parse_element_value input =
    let tag = read input in
    match tag with
    | 'B' | 'C'| 'D'| 'F'| 'I'| 'J'| 'S'| 'Z'| 's' -> ConstValueIndex (read_ui16 input)
    | 'e' -> EnumConstValue
               { type_name_index = read_ui16 input;
                 const_name_index = read_ui16 input;
               }
    | 'c' -> ClassInfoIndex (read_ui16 input)
    | '@' -> AnnotationValue (parse_annotation input)
    | '[' -> ArrayValue (List.init (read_ui16 input) ~f:(fun _ -> parse_element_value input))
    | _ -> raise (Class_format_error "Invalid element value tag")
  in
  let type_index = read_ui16 input in
  let pair_count = read_ui16 input in
  let element_value_pairs = List.init pair_count ~f:(fun _ ->
      { name_index = read_ui16 input;
        value      = parse_element_value input;
      }
    )
  in {type_index; element_value_pairs}

let parse_runtime_annotations input =
  let count = read_ui16 input in
  List.init count ~f:(fun _ -> parse_annotation input)

let parse_runtime_paramater_annotations input =
  let p_count = read_ui16 input in
  List.init p_count ~f:(fun _ -> parse_runtime_annotations input)

let rec parse input ~pool ~count =
  let parse_code input =
    let max_stack = read_ui16 input in
    let max_locals = read_ui16 input in
    let code_len = read_i32 input in
    if code_len <= 0 || code_len >= 65536
    then
      raise (Class_format_error "Invalid Code length")
    else
      let code = Array.init code_len ~f:(fun _ -> read_byte input) in
      let exn_len = read_ui16 input in
      let exn_table = Array.init exn_len ~f:(fun _ ->
          let start_pc = read_ui16 input in
          let end_pc = read_ui16 input in
          let handler_pc = read_ui16 input in
          let catch_type = read_ui16 input in
          { start_pc;end_pc; handler_pc; catch_type }
        ) in
      let attr_count = read_ui16 input in
      let attributes = parse input ~pool:pool ~count:attr_count in
      Code { max_stack; max_locals; code; exn_table; attributes}
  in
  List.init count ~f:(fun _ ->
      let name = ConsPool.get_utf8 pool (read_ui16 input) in
      let len = read_i32 input in
      match name with
        | "ConstantValue" -> parse_constant_value input
        | "Code" -> parse_code input
        | "StackMapTable" -> parse_stack_map_table input
        | "Exceptions" -> parse_exceptions input
        | "InnerClass" -> parse_inner_class input
        | "EnclosingMethod" -> parse_enclosing_method input
        | "Synthetic" -> Synthetic
        | "Signature" -> Signature (read_ui16 input)
        | "SourceFile" -> SourceFile (read_ui16 input)
        | "SourceDebugExtension" -> SourceDebugExtension(List.init len ~f:(
            fun _ -> read_byte input))
        | "LineNumberTable" -> parse_line_number_table input
        | "LocalVariableTable" -> parse_local_variable_table input
        | "LocalVariableTypeTable" -> parse_local_variable_type_table input
        | "Deprecated" -> Deprecated
        | "RuntimeVisibleAnnotations" ->
          RuntimeVisibleAnnotations (parse_runtime_annotations input)
        | "RuntimeInvisibleAnnotation" ->
          RuntimeInvisibleAnnotations (parse_runtime_annotations input)
        | "RuntimeVisibleParameterAnnotations" ->
          RuntimeVisibleParameterAnnotations (parse_runtime_paramater_annotations input)
        | "RuntimeInvisibleParameterAnnotations" ->
          RuntimeInvisibleParameterAnnotations (parse_runtime_paramater_annotations input)
        | _ -> let _ = List.init len ~f:(fun _ -> read_byte input) in Unknown
    )

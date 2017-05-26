open VMError
open Core.Std
open BatIO
open BatIO.BigEndian

let assert_equal len real_len =
  if len <> real_len then raise
      (ClassFormatError (sprintf "Invalid attribute length. Expected:%d Real:%d" len real_len))

let assoc_fold assoc =
  let entries = List.map assoc ~f:(fun (entry, _) -> entry) in
  let len = List.fold_left assoc ~init:0 ~f:(fun acc (_, len) -> acc + len) in
  entries, len

module ConstantValue = struct
  type t = int
  let parse input len = assert_equal len 2; read_ui16 input
end

module Exceptions = struct
  type t = int list

  let parse input len =
    let count = read_ui16 input in
    let real_len = 2 + count * 2 in
    assert_equal len real_len;
    List.init count ~f:(fun _ -> read_ui16 input)
end

module SourceFile = struct
  type t = int
  let parse input len = assert_equal len 2; read_ui16 input;
end

module LineNumberTable = struct
  type line_number =
    { start_pc : int;
      line_number : int;
    }

  type t = line_number list

  let parse input len =
    let count = read_ui16 input in
    let real_len = 2 + count * 4 in
    assert_equal len real_len;
    List.init count ~f:(fun _ ->
        { start_pc = read_ui16 input;
          line_number = read_ui16 input;
        }
      )
end

module LocalVariableTable = struct
  type local_variable =
    { start_pc : int;
      length   : int;
      name_index : int;
      descriptor_index : int;
      index    : int;
    }

  type t = local_variable list

  let parse input len =
    let count = read_ui16 input in
    let real_len = 2 + count * 10 in
    assert_equal len real_len;
    List.init count ~f:(fun _ ->
        { start_pc = read_ui16 input;
          length = read_ui16 input;
          name_index = read_ui16 input;
          descriptor_index = read_ui16 input;
          index = read_ui16 input;
        }
      )
end

module InnerClasses = struct
  type inner_class =
    { inner_class_index : int;
      outer_class_index : int;
      inner_name_index  : int;
      inner_class_access_flags : int;
    }

  type t = inner_class list

  let parse input len =
    let count = read_ui16 input in
    let real_len = 2 + count * 8 in
    assert_equal len real_len;
    List.init count ~f:(fun _ ->
        { inner_class_index = read_ui16 input;
          outer_class_index = read_ui16 input;
          inner_name_index  = read_ui16 input;
          inner_class_access_flags = read_ui16 input;
        }
      )
end

module Synthetic = struct
  type t = unit
  let parse _ len = assert_equal len 0
end

module Deprecated = struct
  type t = unit
  let parse _ len = assert_equal len 0
end

module EnclosingMethod = struct
  type t =
    { class_index : int;
      method_index : int;
    }

  let parse input len =
    assert_equal len 4;
    { class_index = read_ui16 input;
      method_index = read_ui16 input;
    }
end

module Signature = struct
  type t = int
  let parse input len = assert_equal len 2; read_ui16 input
end

module SourceDebugExtension = struct
  type t = int list
  let parse input len = List.init len ~f:(fun _ -> read_byte input)
end

module LocalVariableTypeTable = struct
  type local_variable_type =
    { start_pc : int;
      length   : int;
      name_index : int;
      signature_index : int;
      index    : int;
    }

  type t = local_variable_type list

  let parse input len =
    let count = read_ui16 input in
    let real_len = 2 + count * 10 in
    assert_equal len real_len;
    List.init count ~f:(fun _ ->
        { start_pc = read_ui16 input;
          length = read_ui16 input;
          name_index = read_ui16 input;
          signature_index = read_ui16 input;
          index = read_ui16 input;
        }
      )
end

module Annotation = struct
  type t =
    { type_index : int;
      element_value_pairs : element_value_pair list;
    }
  and element_value_pair =
    { element_name_index : int;
      element_value : element_value;
    }
  and element_value =
    | ConstValueIndex of int
    | EnumConstValue of enum_const_value
    | ClassInfoIndex of int
    | AnnotationValue of t
    | ArrayValue of element_value list
  and enum_const_value =
    { type_name_index : int;
      const_name_index : int;
    }

  let rec parse input =
    let type_index = read_ui16 input in
    let element_value_pairs, len = parse_element_value_pairs input in
    { type_index; element_value_pairs }, len + 2
  and parse_element_value_pairs input =
    let count = read_ui16 input in
    let assoc = List.init count ~f:(fun _ ->
        let name_index = read_ui16 input in
        let ele_value, len = parse_element_value input in
        { element_name_index = name_index;
          element_value      = ele_value;
        }, len + 2
      ) in
    let element_value_pairs, len = assoc_fold assoc in
    element_value_pairs, len + 2
  and parse_element_value input =
    let tag = read input in
    let value, len = match tag with
      | 'B' | 'C'| 'D'| 'F'| 'I'| 'J'| 'S'| 'Z'| 's' ->
        ConstValueIndex (read_ui16 input), 2
      | 'e' -> EnumConstValue
                 { type_name_index = read_ui16 input;
                   const_name_index = read_ui16 input;
                 }, 4
      | 'c' -> ClassInfoIndex (read_ui16 input), 2
      | '@' -> let anno, len = parse input in AnnotationValue anno, len
      | '[' -> let assoc = List.init (read_ui16 input)
                   ~f:(fun _ -> parse_element_value input) in
        let values, len = assoc_fold assoc in
        ArrayValue values, len + 2
      | _ -> raise (ClassFormatError "Invalid element value tag") in
    value, len + 1
end

module TypeAnnotation = struct
  type type_argument_target =
    { offset : int;
      type_argument_index : int;
    }

  type type_parameter_bound_target =
    { type_parameter_index : int;
      bound_index : int;
    }

  type localvar =
    { start_pc : int;
      length   : int;
      index    : int;
    }

  type target_info =
    | TypeParameterTarget of int
    | SupertypeTarget of int
    | TypeParameterBoundTarget of type_parameter_bound_target
    | EmptyTarget
    | MethodFormalParameterTarget of int
    | ThrowsTarget of int
    | LocalvarTarget of localvar list
    | CatchTarget of int
    | OffsetTarget of int
    | TypeArgumentTarget of type_argument_target

  type type_path =
    { type_path_kind : int;
      type_argument_index : int;
    }

  type t =
    { target_type : int;
      target_info : target_info;
      target_path : type_path list;
      type_index  : int;
      element_value_pairs : Annotation.element_value_pair list;
    }

  let parse input =
    let target_type = read_byte input in
    let target_info, info_len = match target_type with
      | t when t = 0x00 || t = 0x01 -> TypeParameterTarget (read_byte input), 1
      | t when t = 0x10 -> SupertypeTarget (read_ui16 input), 2
      | t when t = 0x11 || t = 0x12 -> TypeParameterBoundTarget
                                         { type_parameter_index = read_byte input;
                                           bound_index = read_byte input;
                                         }, 2
      | t when t >= 0x13 && t <= 0x15 -> EmptyTarget, 0
      | t when t = 0x16 -> MethodFormalParameterTarget (read_byte input), 1
      | t when t = 0x17 -> ThrowsTarget (read_ui16 input), 2
      | t when t = 0x40 || t = 0x41 -> let count = read_ui16 input in
        let localvars = List.init count ~f:(fun _ ->
            { start_pc = read_ui16 input;
              length = read_ui16 input;
              index = read_ui16 input;
            }
          ) in LocalvarTarget localvars, 2 + count * 6
      | t when t = 0x42 -> CatchTarget (read_ui16 input), 2
      | t when t >= 0x43 && t <= 0x46 -> OffsetTarget (read_ui16 input), 2
      | t when t >= 0x47 && t <= 0x4B -> TypeArgumentTarget
                                           { offset = read_ui16 input;
                                             type_argument_index = read_byte input;
                                           }, 3
      | _ -> raise (ClassFormatError "Invalid target type") in
    let path_len = read_byte input in
    let target_path = List.init path_len ~f:(fun _ ->
        { type_path_kind = read_byte input;
          type_argument_index = read_byte input;
        }
      ) in
    let type_index = read_ui16 input in
    let element_value_pairs, ele_pair_len = Annotation.parse_element_value_pairs input in
    { target_type;
      target_info;
      target_path;
      type_index;
      element_value_pairs;
    }, 1 + info_len + path_len + 2 + ele_pair_len
end

module RuntimeVisibleAnnotations = struct
  type t = Annotation.t list

  let parse_annotations input=
    let count = read_ui16 input in
    let assoc = List.init count ~f:(fun _ -> Annotation.parse input) in
    let annotations, len = assoc_fold assoc in
    annotations, len + 2

  let parse input len =
    let annotations, real_len = parse_annotations input in
    assert_equal len real_len;
    annotations
end

module RuntimeInvisibleAnnotations = struct
  include RuntimeVisibleAnnotations
end

module RuntimeVisibleParameterAnnotations = struct
  type t = Annotation.t list list

  let parse input len =
    let count = read_byte input in
    let assoc = List.init count ~f:(fun _ ->
        RuntimeVisibleAnnotations.parse_annotations input ) in
    let annotations_list, real_len = assoc_fold assoc in
    assert_equal len (real_len + 1);
    annotations_list
end

module RuntimeInvisibleParameterAnnotations = struct
  include RuntimeVisibleParameterAnnotations
end

module RuntimeVisibleTypeAnnotations = struct
  type t = TypeAnnotation.t list

  let parse input len =
    let count = read_ui16 input in
    let assoc = List.init count ~f:(fun _ -> TypeAnnotation.parse input) in
    let annotations, real_len = assoc_fold assoc in
    assert_equal len (real_len + 2);
    annotations
end

module RuntimeInvisibleTypeAnnotations = struct
  include RuntimeVisibleTypeAnnotations
end

module AnnotationDefault = struct
  type t = Annotation.element_value

  let parse input len =
    let value, real_len = Annotation.parse_element_value input in
    assert_equal len real_len;
    value
end

module BootstrapMethods = struct
  type bootstrap_method =
    { bootstrap_method_ref : int;
      bootstrap_arguments  : int list;
    }

  type t = bootstrap_method list

  let parse input len =
    let count = read_ui16 input in
    let assoc = List.init count ~f:(fun _ ->
        let bootstrap_method_ref = read_ui16 input in
        let arg_count = read_ui16 input in
        let bootstrap_arguments = List.init arg_count
            ~f:(fun _ -> read_ui16 input) in
        { bootstrap_method_ref; bootstrap_arguments }, 2 + 2 + arg_count * 2
      ) in
    let methods, real_len = assoc_fold assoc in
    assert_equal len (real_len + 2);
    methods
end

module MethodParameters = struct
  type parameter =
    { name_index : int;
      access_flags : int;
    }

  type t = parameter list

  let parse input len =
    let count = read_ui16 input in
    let parameters = List.init count ~f:(fun _ ->
        { name_index = read_ui16 input;
          access_flags = read_ui16 input;
        }
      ) in
    let real_len = 1 + count * 4 in
    assert_equal len real_len;
    parameters
end

module StackMapTable = struct
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

  type stack_map_frame =
    | SameFrame
    | SameLocals1StackItemFrame of same_locals_1_stack_item_frame
    | SameLocals1StackItemFrameExtended of same_locals_1_stack_item_frame_extende
    | ChopFrame of chop_frame
    | SameFrameExtended of same_frame_extended
    | AppendFrame of append_frame
    | FullFrame of full_frame

  type t = stack_map_frame list

  let parse input len =
    let parse_vtype input =
      match read_byte input with
      | 0 -> Top, 1
      | 1 -> Integer, 1
      | 2 -> Float, 1
      | 3 -> Double, 1
      | 4 -> Long, 1
      | 5 -> Null, 1
      | 6 -> UninitializedThis, 1
      | 7 -> Object (read_ui16 input), 1 + 2
      | 8 -> Uninitialized (read_ui16 input), 1 + 2
      | _ -> raise (ClassFormatError "Invalid verification_type")
    in
    let parse_vtype_list input count =
      let assoc = Array.init count ~f:(fun _ -> parse_vtype input) in
      let vlist = Array.map assoc ~f:(fun (vt, _) -> vt) in
      let len = Array.fold_right assoc ~init:0 ~f:(fun (_, l) acc -> acc + l) in
      vlist, len
    in
    let parse_frame input =
      match read_byte input with
      | ft when ft >= 0 && ft <= 63 -> SameFrame, 1
      | ft when ft >= 64 && ft <= 127 -> let stack, len = parse_vtype input in
        SameLocals1StackItemFrame { stack }, 1 + len
      | ft when ft = 247 -> let offset_delta = read_ui16 input in
        let stack, len = parse_vtype input in
        SameLocals1StackItemFrameExtended
          { offset_delta;
            stack
          }, 3 + len
      | ft when ft >= 248 && ft <= 250 -> ChopFrame {offset_delta = read_ui16 input}, 3
      | ft when ft = 251 -> SameFrameExtended {offset_delta = read_ui16 input}, 3
      | ft when ft >= 252 && ft <= 254 ->
        let offset_delta = read_ui16 input in
        let count = ft - 251 in
        let locals, real_len = parse_vtype_list input count in
        AppendFrame { offset_delta; locals }, 3 + real_len
      | ft when ft = 255 ->
        let offset_delta = read_ui16 input in
        let locals_count = read_ui16 input in
        let locals, local_len = parse_vtype_list input locals_count in
        let stacks_count = read_ui16 input in
        let stacks, stacks_len = parse_vtype_list input stacks_count in
        FullFrame { offset_delta; locals; stacks}, 3 + 2 + local_len + 2 + stacks_len
      | _ -> raise (ClassFormatError "Invalid frame type")
    in
    let count = read_ui16 input in
    let assoc = List.init count ~f:(fun _ -> parse_frame input) in
    let frames, real_len = assoc_fold assoc in
    assert_equal len (real_len + 2);
    frames
end

module AttrCode = struct
  type t =
    | LineNumberTable        of LineNumberTable.t
    | LocalVariableTable     of LocalVariableTable.t
    | LocalVariableTypeTable of LocalVariableTypeTable.t
    | StackMapTable          of StackMapTable.t
    | RuntimeVisibleTypeAnnotations   of RuntimeVisibleTypeAnnotations.t
    | RuntimeInvisibleTypeAnnotations of RuntimeInvisibleTypeAnnotations.t
    | Unknown

  let parse input pool =
    let attr = Poolbc.get_utf8 pool (read_ui16 input) in
    let len  = read_i32 input in
    let attr = match attr with
      | "LineNumberTable" -> LineNumberTable (LineNumberTable.parse input len)
      | "LocalVariableTable" ->
        LocalVariableTable (LocalVariableTable.parse input len)
      | "LocalVariableTypeTable" ->
        LocalVariableTypeTable (LocalVariableTypeTable.parse input len)
      | "StackMapTable" -> StackMapTable (StackMapTable.parse input len)
      | "RuntimeVisibleTypeAnnotations" ->
        RuntimeVisibleTypeAnnotations (RuntimeVisibleTypeAnnotations.parse input len)
      | "RuntimeInvisibleTypeAnnotations" ->
        RuntimeInvisibleTypeAnnotations (RuntimeInvisibleTypeAnnotations.parse input len)
      | _ -> let _ = List.init len ~f:(fun _ -> read_byte input) in Unknown
    in attr, len + 2 + 4
end

module Code = struct
  type code_exn =
    { start_pc   : int;
      end_pc     : int;
      handler_pc : int;
      catch_type : int;
    }

  type t = { max_stack : int;
             max_locals : int;
             code : char array;
             exn_table : code_exn list;
             attributes : AttrCode.t list;
           }

  let parse input ~pool ~len =
    let parse_attr_list input ~pool ~count =
      let assoc = List.init count ~f:(fun _ -> AttrCode.parse input pool) in
      let attrs = List.filter_map assoc ~f:(fun (a, _) ->
          if a = AttrCode.Unknown then None else Some a ) in
      let len = List.fold_left assoc ~init:0 ~f:(fun acc (_, l) -> acc + l) in
      attrs, len
    in
    let max_stack = read_ui16 input in
    let max_locals = read_ui16 input in
    let code_len = read_i32 input in
    if code_len <= 0 || code_len >= 65536
    then
      raise (ClassFormatError "Invalid Code length")
    else
      let code = Array.init code_len ~f:(fun _ -> Char.of_int_exn @@ read_byte input) in
      let exn_len = read_ui16 input in
      let exn_table = List.init exn_len ~f:(fun _ ->
          let start_pc = read_ui16 input in
          let end_pc = read_ui16 input in
          let handler_pc = read_ui16 input in
          let catch_type = read_ui16 input in
          { start_pc;end_pc; handler_pc; catch_type }
        ) in
      let attr_count = read_ui16 input in
      let attributes, attr_len = parse_attr_list input ~pool:pool ~count:attr_count in
      assert_equal len (2 + 2 + 4 + code_len * 1 + 2 + exn_len * 8 + 2 + attr_len);
      { max_stack; max_locals; code; exn_table; attributes}
end

module AttrClass = struct
  type t =
    | SourceFile              of SourceFile.t
    | InnerClasses            of InnerClasses.t
    | EnclosingMethod         of EnclosingMethod.t
    | SourceDebugExtension    of SourceDebugExtension.t
    | BootstrapMethods        of BootstrapMethods.t
    | Synthetic               of Synthetic.t
    | Deprecated              of Deprecated.t
    | Signature               of Signature.t
    | RuntimeVisibleAnnotations     of RuntimeVisibleAnnotations.t
    | RuntimeInvisibleAnnotations   of RuntimeInvisibleAnnotations.t
    | RuntimeVisibleTypeAnnotations of RuntimeVisibleTypeAnnotations.t
    | RuntimeInvisibleTypeAnnotations  of RuntimeInvisibleTypeAnnotations.t
    | Unknown

  let parse input pool =
    let attr = Poolbc.get_utf8 pool (read_ui16 input) in
    let len  = read_i32 input in
    match attr with
    | "SourceFile" -> SourceFile (SourceFile.parse input len)
    | "InnerClasses" -> InnerClasses (InnerClasses.parse input len)
    | "EnclosingMethod" -> EnclosingMethod (EnclosingMethod.parse input len)
    | "SourceDebugExtension" -> SourceDebugExtension (SourceDebugExtension.parse input len)
    | "BootstrapMethods" -> BootstrapMethods (BootstrapMethods.parse input len)
    | "Synthetic" -> Synthetic (Synthetic.parse input len)
    | "Deprecated" -> Deprecated (Deprecated.parse input len)
    | "Signature" -> Signature (Signature.parse input len)
    | "RuntimeVisibleAnnotations" ->
      RuntimeVisibleAnnotations (RuntimeVisibleAnnotations.parse input len)
    | "RuntimeInvisibleAnnotations" ->
      RuntimeInvisibleAnnotations (RuntimeInvisibleAnnotations.parse input len)
    | "RuntimeVisibleTypeAnnotations" ->
      RuntimeVisibleTypeAnnotations (RuntimeVisibleTypeAnnotations.parse input len)
    | "RuntimeInvisibleTypeAnnotations" ->
      RuntimeInvisibleTypeAnnotations (RuntimeInvisibleTypeAnnotations.parse input len)
    | _ -> let _ = List.init len ~f:(fun _ -> read_byte input) in Unknown
end

module AttrMethod = struct
  type t =
    | Code of Code.t
    | Exceptions of Exceptions.t
    | RuntimeVisibleParameterAnnotations of RuntimeVisibleParameterAnnotations.t
    | RuntimeInvisibleParameterAnnotations of RuntimeInvisibleParameterAnnotations.t
    | AnnotationDefault of AnnotationDefault.t
    | MethodParameters of MethodParameters.t
    | Synthetic of Synthetic.t
    | Deprecated of Deprecated.t
    | Signature of Signature.t
    | RuntimeVisibleAnnotations of RuntimeVisibleAnnotations.t
    | RuntimeInvisibleAnnotations of RuntimeInvisibleAnnotations.t
    | RuntimeVisibleTypeAnnotations of RuntimeVisibleTypeAnnotations.t
    | RuntimeInvisibleTypeAnnotations of RuntimeInvisibleTypeAnnotations.t
    | Unknown

  let parse input pool =
    let attr = Poolbc.get_utf8 pool (read_ui16 input) in
    let len  = read_i32 input in
    match attr with
    | "Code" -> Code (Code.parse input ~len:len ~pool:pool)
    | "Exceptions" -> Exceptions (Exceptions.parse input len)
    | "RuntimeVisibleParameterAnnotations" ->
      RuntimeVisibleParameterAnnotations (RuntimeVisibleParameterAnnotations.parse input len)
    | "RuntimeInvisibleParameterAnnotations" ->
      RuntimeInvisibleParameterAnnotations (RuntimeInvisibleParameterAnnotations.parse input len)
    | "AnnotationDefault" -> AnnotationDefault (AnnotationDefault.parse input len)
    | "MethodParameters" -> MethodParameters (MethodParameters.parse input len)
    | "Synthetic" -> Synthetic (Synthetic.parse input len)
    | "Deprecated" -> Deprecated (Deprecated.parse input len)
    | "Signature" -> Signature (Signature.parse input len)
    | "RuntimeVisibleAnnotations" ->
      RuntimeVisibleAnnotations (RuntimeVisibleAnnotations.parse input len)
    | "RuntimeInvisibleAnnotations" ->
      RuntimeInvisibleAnnotations (RuntimeInvisibleAnnotations.parse input len)
    | "RuntimeVisibleTypeAnnotations" ->
      RuntimeVisibleTypeAnnotations (RuntimeVisibleTypeAnnotations.parse input len)
    | "RuntimeInvisibleTypeAnnotations" ->
      RuntimeInvisibleTypeAnnotations (RuntimeInvisibleTypeAnnotations.parse input len)
    | _ -> let _ = List.init len ~f:(fun _ -> read_byte input) in Unknown
end

module AttrField = struct
  type t =
    | ConstantValue of ConstantValue.t
    | Synthetic of Synthetic.t
    | Deprecated of Deprecated.t
    | Signature of Signature.t
    | RuntimeVisibleAnnotations of RuntimeVisibleAnnotations.t
    | RuntimeInvisibleAnnotations of RuntimeInvisibleAnnotations.t
    | RuntimeVisibleTypeAnnotations of RuntimeVisibleTypeAnnotations.t
    | RuntimeInvisibleTypeAnnotations of RuntimeInvisibleTypeAnnotations.t
    | Unknown

  let parse input pool =
    let attr = Poolbc.get_utf8 pool (read_ui16 input) in
    let len  = read_i32 input in
    match attr with
    | "ConstantValue" -> ConstantValue (ConstantValue.parse input len)
    | "Synthetic" -> Synthetic (Synthetic.parse input len)
    | "Deprecated" -> Deprecated (Deprecated.parse input len)
    | "Signature" -> Signature (Signature.parse input len)
    | "RuntimeVisibleAnnotations" ->
      RuntimeVisibleAnnotations (RuntimeVisibleAnnotations.parse input len)
    | "RuntimeInvisibleAnnotations" ->
      RuntimeInvisibleAnnotations (RuntimeInvisibleAnnotations.parse input len)
    | "RuntimeVisibleTypeAnnotations" ->
      RuntimeVisibleTypeAnnotations (RuntimeVisibleTypeAnnotations.parse input len)
    | "RuntimeInvisibleTypeAnnotations" ->
      RuntimeInvisibleTypeAnnotations (RuntimeInvisibleTypeAnnotations.parse input len)
    | _ -> let _ = List.init len ~f:(fun _ -> read_byte input) in Unknown
end

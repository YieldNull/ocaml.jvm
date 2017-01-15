val assert_equal : int -> int -> unit

module ConstantValue : sig
  type t = int
  val parse : BatInnerIO.input -> int -> t
end

module Exceptions : sig
  type t = int list
  val parse : BatInnerIO.input -> int -> t
end

module SourceFile : sig
  type t = int
  val parse : BatInnerIO.input -> int -> t
end

module LineNumberTable : sig
  type line_number =
    { start_pc : int;
      line_number : int;
    }

  type t = line_number list
  val parse : BatInnerIO.input -> int -> t
end

module LocalVariableTable : sig
  type local_variable =
    { start_pc : int;
      length   : int;
      name_index : int;
      descriptor_index : int;
      index    : int;
    }

  type t = local_variable list
  val parse : BatInnerIO.input -> int -> t
end

module InnerClasses : sig
  type inner_class =
    { inner_class_index : int;
      outer_class_index : int;
      inner_name_index  : int;
      inner_class_access_flags : int;
    }

  type t = inner_class list
  val parse : BatInnerIO.input -> int -> t
end

module Synthetic : sig
  type t = unit
  val parse : BatInnerIO.input -> int -> t
end

module Deprecated : sig
  type t = unit
  val parse : BatInnerIO.input -> int -> t
end

module EnclosingMethod : sig
  type t =
    { class_index : int;
      method_index : int;
    }

  val parse : BatInnerIO.input -> int -> t
end

module Signature : sig
  type t = int
  val parse : BatInnerIO.input -> int -> t
end

module SourceDebugExtension : sig
  type t = int list
  val parse : BatInnerIO.input -> int -> t
end

module LocalVariableTypeTable : sig
  type local_variable_type =
    { start_pc : int;
      length   : int;
      name_index : int;
      signature_index : int;
      index    : int;
    }

  type t = local_variable_type list
  val parse : BatInnerIO.input -> int -> t
end

module Annotation : sig
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

  val parse : BatInnerIO.input -> t * int
end

module TypeAnnotation : sig
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

  val parse : BatInnerIO.input -> t * int
end

module RuntimeVisibleAnnotations : sig
  type t = Annotation.t list
  val parse : BatInnerIO.input -> int -> t
end

module RuntimeInvisibleAnnotations : sig
  type t = Annotation.t list
  val parse : BatInnerIO.input -> int -> t
end

module RuntimeVisibleParameterAnnotations : sig
  type t = Annotation.t list list
  val parse : BatInnerIO.input -> int -> t
end

module RuntimeInvisibleParameterAnnotations : sig
  type t = Annotation.t list list
  val parse : BatInnerIO.input -> int -> t
end

module RuntimeVisibleTypeAnnotations : sig
  type t = TypeAnnotation.t list
  val parse : BatInnerIO.input -> int -> t
end

module RuntimeInvisibleTypeAnnotations : sig
  type t = TypeAnnotation.t list
  val parse : BatInnerIO.input -> int -> t
end

module AnnotationDefault : sig
  type t = Annotation.element_value
  val parse : BatInnerIO.input -> int -> t
end

module BootstrapMethods : sig
  type bootstrap_method =
    { bootstrap_method_ref : int;
      bootstrap_arguments  : int list;
    }

  type t = bootstrap_method list
  val parse : BatInnerIO.input -> int -> t
end

module MethodParameters : sig
  type parameter =
    { name_index : int;
      access_flags : int;
    }

  type t = parameter list
  val parse : BatInnerIO.input -> int -> t
end

module StackMapTable : sig
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
  val parse : BatInnerIO.input -> int -> t
end

module AttrCode : sig
  type t =
    | LineNumberTable        of LineNumberTable.t
    | LocalVariableTable     of LocalVariableTable.t
    | LocalVariableTypeTable of LocalVariableTypeTable.t
    | StackMapTable          of StackMapTable.t
    | RuntimeVisibleTypeAnnotations   of RuntimeVisibleTypeAnnotations.t
    | RuntimeInvisibleTypeAnnotations of RuntimeInvisibleTypeAnnotations.t
    | Unknown

  val parse : BatInnerIO.input -> Cons_pool.t -> t * int
end

module Code : sig
  type code_exn =
    { start_pc   : int;
      end_pc     : int;
      handler_pc : int;
      catch_type : int;
    }

  type t = { max_stack : int;
             max_locals : int;
             code : int list;
             exn_table : code_exn list;
             attributes : AttrCode.t list;
           }

  val parse : BatInnerIO.input -> pool:Cons_pool.t -> len:int -> t
end

module AttrClass : sig
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

  val parse : BatInnerIO.input -> Cons_pool.t -> t
end

module AttrMethod : sig
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

  val parse : BatInnerIO.input -> Cons_pool.t -> t
end

module AttrField : sig
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

  val parse : BatInnerIO.input -> Cons_pool.t -> t
end

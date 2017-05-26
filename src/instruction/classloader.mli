
val root_class : unit -> Jclass.t

(* load a class from bytecode. do not use it for resolving entries in constant_pool *)
val load_class : Jloader.t -> string -> Jclass.t

(* resolve a class in constant_pool *)
val resolve_class : Jloader.t -> caller:string -> name:string -> Jclass.t

(* resolve a field in constant_pool *)
val resolve_field : Jclass.t -> string -> MemberID.t -> Jfield.t

(* resolve a method of class in constant_pool *)
val resolve_method_of_class : Jclass.t -> string -> MemberID.t -> Jmethod.t

(* resolve a method of interface in constant_pool *)
val resolve_method_of_interface : Jclass.t -> string -> MemberID.t -> Jmethod.t

open Core.Std

val modified_utf8_to_unicode : string -> string
val modified_utf8_to_uchar_arr : string -> (Jvalue.t) Array.t
val uchar_arr_to_modified_utf8 : (Jvalue.t) Array.t -> string

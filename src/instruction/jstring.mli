open Jvalue

val of_string : string -> jobject

val of_uchars : Jvalue.t array -> jobject

val find : string -> jobject option

val find_or_create : string -> jobject

val intern : jobject -> jobject

val compare : jobject -> jobject -> int

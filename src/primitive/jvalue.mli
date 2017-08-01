open Core
open Types

type jbyte = int
type jshort = int
type jchar = int
type jint = int32
type jlong = int64
type jfloat = Float32.t
type jdouble = float
type jbool = int

type jobject = InnValue.jobject =
  { jclass : InnClass.t;
    fields : (MemberID.t, InnValue.t) Hashtbl.t;
  }

type jarray = InnValue.jarray =
  { jclass : InnClass.t;
    values : (InnValue.t) Array.t;
  }

type t = InnValue.t =
  | Byte of jbyte
  | Short of jshort
  | Char of jchar
  | Int of jint
  | Float of jfloat
  | Long of jlong
  | Double of jdouble
  | Boolean of jbool
  | Object of jobject
  | Array of jarray
  | Null
  | ReturnAddress

val get_int : t -> jint

val get_short : t -> jshort

val get_byte : t -> jbyte

val get_char : t -> jchar

val get_boolean : t -> jbool

val get_long : t -> jlong

val get_float : t -> jfloat

val get_double : t -> jdouble

val get_reference : t -> t

val get_reference_or_return_address : t -> t

val get_object : t -> jobject

val get_array : t -> jarray

val must_be_int : t -> unit

val must_be_long : t -> unit

val must_be_float : t -> unit

val must_be_double : t -> unit

val must_be_reference : t -> unit

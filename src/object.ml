open Core.Std

type t =
  { jclass : Jclass.t;
    fields : (string, value) Hashtbl.t;
  }
and value =
  | Byte of int
  | Short of int
  | Char of int
  | Int of int32
  | Float of float
  | Long of int64
  | Double of float
  | Boolean of int
  | Reference of t
  | Null

let create jclass = ()

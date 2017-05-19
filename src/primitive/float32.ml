type float32
type t = float32

external literal_zero : unit -> float32 = "float32_zero"
external literal_one : unit -> float32 = "float32_one"

external add : float32 -> float32 -> float32 = "float32_add"
external sub : float32 -> float32 -> float32 = "float32_sub"
external mul : float32 -> float32 -> float32 = "float32_mul"
external div : float32 -> float32 -> float32 = "float32_div"
external rem : float32 -> float32 -> float32 = "float32_rem"

external is_nan : float32 -> bool = "float32_is_nan"

external neg : float32 -> float32 = "float32_neg"
external equal : float32 -> float32 -> bool = "float32_equal"
external compare : float32 -> float32 -> int = "float32_compare"

external to_int32 : float32 -> int32 = "float32_to_int32"
external to_int64 : float32 -> int64 = "float32_to_int64"
external to_float64 : float32 -> float = "float32_to_float64"

external of_int32_bits : int32 -> float32 = "float32_of_int32_bits"
external of_int32 : int32 -> float32 = "float32_of_int32"
external of_int64 : int64 -> float32 = "float32_of_int64"
external of_float64 : float -> float32 = "float32_of_float64"

let one = literal_one ()

let zero = literal_zero ()

let nan = of_int32_bits 0x7fc00000l

let positive_infinity = of_int32_bits 0x7f800000l

let negative_infinity = of_int32_bits 0xff800000l

let (>=.) f1 f2 = compare f1 f2 >= 0

let (<=.) f1 f2 = compare f1 f2 <= 0

let (=.) f1 f2 = compare f1 f2 = 0

let (<.) f1 f2 = compare f1 f2 < 0

let (>.) f1 f2 = compare f1 f2 > 0

let (<>.) f1 f2 = compare f1 f2 <> 0

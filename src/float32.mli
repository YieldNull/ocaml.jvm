type float32
type t = float32

external of_int32 : int32 -> float32 = "float32_of_int32"
external to_double : float32 -> float = "float32_to_double"

external add : float32 -> float32 -> float32 = "float32_add"
external sub : float32 -> float32 -> float32 = "float32_sub"
external mul : float32 -> float32 -> float32 = "float32_mul"
external div : float32 -> float32 -> float32 = "float32_div"
external rem : float32 -> float32 -> float32 = "float32_rem"
external equal : float32 -> float32 -> bool = "float32_equal"
external neg : float32 -> float32 = "float32_neg"

val zero : float32
val one : float32

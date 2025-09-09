
type _ typed_value =
  | Int : int -> int typed_value
  | Str : string -> string typed_value


let get_value_gadt : type a. a typed_value -> a = function
  | Int i -> i
  | Str s -> s

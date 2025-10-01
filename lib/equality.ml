module type EQUATABLE = sig
  type t
  val equal : t -> t -> bool
end

module MakeDefaultEq (T: sig type t end) : EQUATABLE with type t = T.t = struct
  type t = T.t
  let equal = (=)
end

module IntEq = MakeDefaultEq(struct type t = int end)
module FloatEq = MakeDefaultEq(struct type t = float end)
module BoolEq = MakeDefaultEq(struct type t = bool end)
module CharEq = MakeDefaultEq(struct type t = char end)
module StringEq = MakeDefaultEq(struct type t = string end)

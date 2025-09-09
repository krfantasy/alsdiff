module Fraction =
  struct
    type t = {
      p : int;
      q : int;
    }

    let make p q =
      if q = 0 then raise (Invalid_argument "Denominator can't be 0")
      else { p; q }

    let numerator t = t.p
    let demoninator t = t.q

    let to_string t =
      Printf.sprintf "%d/%d" t.p t.q

    let to_float t =
      (float_of_int t.p) /. (float_of_int t.q)

    let add f1 f2 =
      make (f1.p * f2.q + f2.p * f1.q) (f1.q * f2.q)
  end

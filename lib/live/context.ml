
module IntHash = struct
  type t = int
  let equal i j = i = j
  let hash i = i land max_int
end

module IntHashtbl = Hashtbl.Make(IntHash)

type t = {
  mutable pointees : string IntHashtbl.t; (* (id, name) *)
}

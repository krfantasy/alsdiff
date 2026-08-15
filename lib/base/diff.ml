open Equality

module type PATCH = sig
  type t
  val is_empty : t -> bool
end

module type DIFFABLE_ID = sig
  type t
  include IDENTIFIABLE with type t := t

  module Patch : PATCH

  val diff : t -> t -> Patch.t
end

module type DIFFABLE_EQ = sig
  type t
  include EQUALABLE with type t := t

  module Patch : PATCH

  val diff : t -> t -> Patch.t
end


(** Phantom types for distinguishing change kinds *)
type atomic
type structured

(** The unified type to describe the change of a value.

    This polymorphic variant type represents different kinds of changes that can occur
    when comparing two values. The ['kind] parameter is a phantom type that distinguishes
    between atomic and structured changes at the type level.

    @param 'a The type of the value being changed
    @param 'p The type of the patch/modify information
    @param 'kind The phantom type (either [atomic] or [structured])

    TODO: adding a [`Moved] or [`Reordered] variant,
    currently the Myers diff algorithm can't really detect an item moved/reordered in a sequence.
*)
type ('a, 'p, 'kind) change = [
  | `Unchanged  (** The value remained the same *)
  | `Added of 'a  (** A new value was added *)
  | `Removed of 'a  (** A value was removed *)
  | `Modified of 'p  (** A value was modified, with patch information [p] *)
]

(** A patch representing the change of a simple atomic value.

    Atomic patches are used for primitive values that can be directly compared
    and replaced, such as integers, strings, floats, or booleans.

    @param oldval The original value before the change
    @param newval The new value after the change
*)
type 'a atomic_patch = { oldval : 'a; newval : 'a }

(** A change type for atomic values with compile-time type safety.

    This type ensures that only atomic changes (changes to simple/primitive values)
    can be used, preventing accidental mixing with structured changes at compile time.

    @param 'a The type of the atomic value (e.g., int, string, float)
*)
type 'a atomic_change = ('a, 'a atomic_patch, atomic) change

(** A change type for structured values with compile-time type safety.

    This type is used for complex objects that have their own Patch.t type
    and require nested diffing. Examples include Loop, Send, Device, etc.
    The phantom type [structured] prevents mixing with atomic changes.

    @param 'a The type of the structured value
    @param 'p The patch type for the structured value
*)
type ('a, 'p) structured_change = ('a, 'p, structured) change

(** A type representing updates to complex structured objects.

    Updates are used when a complex object has been modified internally,
    with the patch describing the specific changes made to the object's structure.

    @param 'p The patch type describing the modifications
    @param 'kind The phantom type (either [atomic] or [structured])
*)
type ('p, 'kind) update = [
  | `Unchanged  (** The object remained unchanged *)
  | `Modified of 'p  (** The object was modified according to patch [p] *)
]

(** An update type specifically for structured objects.

    This type ensures that only structured updates (updates to complex objects)
    can be used, providing compile-time guarantees about the type of update.

    @param 'p The patch type for the structured object
*)
type 'p structured_update = ('p, structured) update

(** An update type specifically for atomic values.

    This type ensures that only atomic updates (updates to primitive values)
    can be used, providing compile-time type safety.

    @param 'a The type of the atomic value
*)
type 'a atomic_update = ('a atomic_patch, atomic) update


let diff_value ~equal ~diff old_value new_value =
  if equal old_value new_value then
    `Unchanged
  else
    `Modified (diff old_value new_value)

let diff_value_opt ~diff_some old_value new_value =
  match (old_value, new_value) with
  | (Some oldval, None) -> `Removed oldval
  | (None, Some newval) -> `Added newval
  | (Some oldval, Some newval) -> diff_some oldval newval
  | (None, None) -> `Unchanged

let diff_atomic_value (module EQ : EQUALABLE)
    (old_value : EQ.t) (new_value : EQ.t) : EQ.t atomic_update =
  diff_value ~equal:EQ.equal ~diff:(fun oldval newval -> { oldval; newval }) old_value new_value

let diff_atomic_value_opt (module EQ : EQUALABLE)
    (old_value : EQ.t option) (new_value : EQ.t option) : EQ.t atomic_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_atomic_value (module EQ) o n :> EQ.t atomic_change))
    old_value new_value


let diff_complex_value (module EQ : DIFFABLE_EQ)
    (old_value : EQ.t)
    (new_value : EQ.t) : EQ.Patch.t structured_update =
  diff_value ~equal:EQ.equal ~diff:EQ.diff old_value new_value

let diff_complex_value_id (module ID : DIFFABLE_ID)
    (old_value : ID.t)
    (new_value : ID.t) : ID.Patch.t structured_update =
  if ID.has_same_id old_value new_value then
    let patch = ID.diff old_value new_value in
    if ID.Patch.is_empty patch then `Unchanged
    else `Modified patch
  else
    failwith "diff_complex_value_id: IDs do not match"

let diff_complex_value_opt (module EQ : DIFFABLE_EQ)
    (old_value : EQ.t option)
    (new_value : EQ.t option) : (EQ.t, EQ.Patch.t) structured_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_complex_value (module EQ) o n :> (EQ.t, EQ.Patch.t) structured_change))
    old_value new_value


let diff_complex_value_id_opt (module ID : DIFFABLE_ID)
    (old_value : ID.t option)
    (new_value : ID.t option) : (ID.t, ID.Patch.t) structured_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_complex_value_id (module ID) o n :> (ID.t, ID.Patch.t) structured_change))
    old_value new_value


(** Check if a change or update represents no actual modification.

    This unified function works with both change and update types by using
    polymorphic variants to handle both cases safely.

    For structured changes:
    {[
      let unchanged = is_unchanged_change (module MyPatch) structured_change
    ]}

    For structured updates:
    {[
      let unchanged = is_unchanged_update (module MyPatch) structured_update
    ]}

    @param P A PATCH module for the patch type
    @return true if the operation represents no modification
*)
let is_unchanged_change (module P : PATCH)
    (operation : (_, P.t, structured) change) : bool =
  match operation with
  | `Added _ | `Removed _ -> false
  | `Unchanged -> true
  | `Modified p -> P.is_empty p

(** Check if an update represents no actual modification.

    This function specifically handles update types which lack Added/Removed variants.

    @param P A PATCH module for the patch type
    @param operation The update to check
    @return true if the update represents no modification
*)
let is_unchanged_update (module P : PATCH)
    (operation : (P.t, structured) update) : bool =
  match operation with
  | `Unchanged -> true
  | `Modified p -> P.is_empty p


(** Check if an atomic change represents no actual modification.

    For atomic changes, the patch equality is checked using direct value comparison
    since atomic types don't have corresponding PATCH modules.

    @param c The atomic change to check
    @return true if the change represents no modification
*)
let is_unchanged_atomic_change (type a) (c : a atomic_change) : bool =
  is_unchanged_change
    (module struct
      type t = a atomic_patch
      let is_empty {oldval; newval} = oldval = newval
    end)
    c

(** Check if an atomic update represents no actual modification.

    For atomic updates, the patch equality is checked using direct value comparison
    since atomic types don't have corresponding PATCH modules.

    @param u The atomic update to check
    @return true if the update represents no modification
*)
let is_unchanged_atomic_update (type a) (u : a atomic_update) : bool =
  is_unchanged_update
    (module struct
      type t = a atomic_patch
      let is_empty {oldval; newval} = oldval = newval
    end)
    u


(* Module type for a hashable type, used by diff_set_generic *)
module type HASHER = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end


(** Generic Myers algorithm for ordered list diffing (linear-space variant).

    Divide-and-conquer "middle snake" refinement (Myers 1986 §4; implementation
    follows J. Coglan, "Myers diff in linear space"). Finds the middle snake of
    each sub-region, recurses on the two halves, then reconstructs the edit
    script by walking the resulting point path.

    Time:  O((N+M) * D) where D is the edit distance.
    Space: O(N+M) — two V vectors per recursion level, whose total along any
           root-to-leaf path is O(N+M).

    Multiple equally-minimal edit scripts may exist; this picks a deterministic
    path that can differ from a quadratic backtracking variant.

    @param compare Equality predicate over elements
    @param on_match Produce the change for a matched (diagonal) element pair
    @param old_list Original list
    @param new_list Modified list
    @return List of changes representing a minimal edit script
*)
let diff_list_generic (type a p k)
    ~(compare: a -> a -> bool)
    ~(on_match: a -> a -> (a, p, k) change)
    (old_list : a list) (new_list : a list) : (a, p, k) change list =
  let a = Array.of_list old_list in
  let b = Array.of_list new_list in
  let n = Array.length a in
  let m = Array.length b in

  (* Trivial edge cases *)
  if n = 0 then List.map (fun x -> `Added x) new_list
  else if m = 0 then List.map (fun x -> `Removed x) old_list
  else

    (* Find the middle snake of box [left, right) x [top, bottom).
       Returns Some ((x1, y1), (x2, y2)) — the snake's start and end — or None
       when the box is a single point. Coordinates are absolute indices into [a]
       and [b]. Forward scan maximises x from (left, top); backward scan minimises
       y from (right, bottom). vf is indexed by k, vb by c = k - delta. *)
    let midpoint left top right bottom =
      let width = right - left in
      let height = bottom - top in
      let size = width + height in
      if size = 0 then None
      else
        let delta = width - height in
        let max_d = (size + 1) / 2 in
        let vf = Array.make (2 * max_d + 1) 0 in
        let vb = Array.make (2 * max_d + 1) 0 in
        vf.(1 + max_d) <- left;
        vb.(1 + max_d) <- bottom;
        let found = ref None in
        let d = ref 0 in
        while !found = None && !d <= max_d do
          let d0 = !d in
          (* Forward pass: maximise x from (left, top). k descends so the
             overlap fires on the uppermost diagonal, matching Git/Coglan. *)
          let k = ref d0 in
          while !found = None && !k >= -d0 do
            let kf = !k in
            let px, x =
              if kf = -d0 || (kf <> d0 && vf.(kf - 1 + max_d) < vf.(kf + 1 + max_d)) then
                let x = vf.(kf + 1 + max_d) in
                (x, x)
              else
                let px = vf.(kf - 1 + max_d) in
                (px, px + 1)
            in
            let y = top + (x - left) - kf in
            let py = if d0 = 0 || x <> px then y else y - 1 in
            let x2 = ref x in
            let y2 = ref y in
            while !x2 < right && !y2 < bottom && compare a.(!x2) b.(!y2) do
              incr x2;
              incr y2
            done;
            vf.(kf + max_d) <- !x2;
            (* Overlap with the reverse scan is detected only when delta is odd. *)
            let c = kf - delta in
            if delta land 1 <> 0 && c >= -d0 + 1 && c <= d0 - 1 && !y2 >= vb.(c + max_d) then
              found := Some ((px, py), (!x2, !y2));
            k := !k - 2
          done;
          (* Backward pass: minimise y from (right, bottom).
             Explored largest c first so the overlap fires at the largest k,
             matching the old quadratic algorithm's delete-first tie-breaking. *)
          let c = ref d0 in
          while !found = None && !c >= -d0 do
            let cf = !c in
            let k = cf + delta in
            let py, y =
              if cf = -d0 || (cf <> d0 && vb.(cf - 1 + max_d) > vb.(cf + 1 + max_d)) then
                let y = vb.(cf + 1 + max_d) in
                (y, y)
              else
                let py = vb.(cf - 1 + max_d) in
                (py, py - 1)
            in
            let x = left + (y - top) + k in
            let px = if d0 = 0 || y <> py then x else x + 1 in
            let x2 = ref x in
            let y2 = ref y in
            while !x2 > left && !y2 > top && compare a.(!x2 - 1) b.(!y2 - 1) do
              decr x2;
              decr y2
            done;
            vb.(cf + max_d) <- !y2;
            (* Overlap with the forward scan is detected only when delta is even. *)
            if delta land 1 = 0 && k >= -d0 && k <= d0 && !x2 <= vf.(k + max_d) then
              found := Some ((!x2, !y2), (px, py));
            c := !c - 2
          done;
          incr d
        done;
        !found
    in

    (* Recursively collect the snake endpoints along the optimal path. *)
    let rec find_path left top right bottom =
      match midpoint left top right bottom with
      | None -> []
      | Some (start, finish) ->
        let sx, sy = start in
        let fx, fy = finish in
        let head = find_path left top sx sy in
        let tail = find_path fx fy right bottom in
        let head' = if head = [] then [start] else head in
        let tail' = if tail = [] then [finish] else tail in
        head' @ tail'
    in

    (* Emit diagonal matches between (x1, y1) and (x2, y2); returns where it
       stopped. Results are prepended to [acc] and reversed once at the end. *)
    let emit_diagonal x1 y1 x2 y2 acc =
      let xx = ref x1 in
      let yy = ref y1 in
      while !xx < x2 && !yy < y2 && compare a.(!xx) b.(!yy) do
        acc := on_match a.(!xx) b.(!yy) :: !acc;
        incr xx;
        incr yy
      done;
      (!xx, !yy)
    in

    (* Derive the single edit between two consecutive path points (a snake is one
       right/down step bracketed by diagonals). *)
    let walk_pair (x1, y1) (x2, y2) acc =
      let x1, y1 = emit_diagonal x1 y1 x2 y2 acc in
      let x1, y1 =
        if x2 - x1 < y2 - y1 then begin
          acc := `Added b.(y1) :: !acc;
          (x1, y1 + 1)
        end else if x2 - x1 > y2 - y1 then begin
          acc := `Removed a.(x1) :: !acc;
          (x1 + 1, y1)
        end else
          (x1, y1)
      in
      ignore (emit_diagonal x1 y1 x2 y2 acc)
    in

    let path = find_path 0 0 n m in
    let acc = ref [] in
    let rec walk = function
      | [] | [_] -> ()
      | p1 :: ((p2 :: _) as rest) ->
        walk_pair p1 p2 acc;
        walk rest
    in
    walk path;
    List.rev !acc


(** Myers' O(ND) diff algorithm with equality-based matching.

    Delegates to [diff_list_generic], the linear-space middle-snake variant of
    Myers' algorithm (1986 §4). Returns a list of changes representing the
    shortest edit script.
    Time complexity: O((N+M)D) where D is the size of the edit script.
    Space complexity: O(N+M) working space, plus the output list.
*)
let diff_list (type k) (module EQ : DIFFABLE_EQ) (old_list : EQ.t list) (new_list : EQ.t list)
  : (EQ.t, EQ.Patch.t, k) change list =
  diff_list_generic
    ~compare:EQ.equal
    ~on_match:(fun old_item new_item ->
        if EQ.equal old_item new_item then
          `Unchanged
        else
          `Modified (EQ.diff old_item new_item)
      )
    old_list new_list

let diff_list_id (type k) (module ID : DIFFABLE_ID) (old_list : ID.t list) (new_list : ID.t list)
  : (ID.t, ID.Patch.t, k) change list =
  diff_list_generic
    ~compare:ID.has_same_id
    ~on_match:(fun old_item new_item ->
        let patch = ID.diff old_item new_item in
        if ID.Patch.is_empty patch then
          `Unchanged
        else
          `Modified patch
      )
    old_list new_list

(* Utility functions *)
let update_of_patch (module P : PATCH)
    (x : P.t) : P.t structured_update =
  if P.is_empty x then
    `Unchanged
  else
    `Modified x

let update_of_atomic (module D : DIFFABLE_EQ)
    (x : D.t atomic_update) : D.Patch.t structured_update =
  match x with
  | `Modified { oldval; newval } -> `Modified (D.diff oldval newval)
  | `Unchanged -> `Unchanged

let structured_of_atomic (module D : DIFFABLE_EQ)
    (x : D.t atomic_change) : (D.t, D.Patch.t) structured_change =
  match x with
  | `Added a -> `Added a
  | `Removed a -> `Removed a
  | `Unchanged -> `Unchanged
  | `Modified { oldval; newval } -> `Modified (D.diff oldval newval)


(** Post-process a change list to merge adjacent Removed+Added pairs into Modified.

    This enables replacement detection for anonymous sequences (without IDs) by
    converting patterns like [`Removed old; `Added new] into [`Modified patch].

    The merging only happens for immediately adjacent pairs. For example:
    - [`Removed 1; `Added 2; `Unchanged] becomes [`Modified {1,2}; `Unchanged]
    - [`Removed 1; `Unchanged; `Added 2] stays unchanged (not adjacent)

    @param diff Function to create a patch from old and new values
    @param changes The change list from Myers diff
    @return Change list with adjacent Removed+Added pairs merged into Modified
*)
let merge_adjacent_changes (type a p k)
    ~(diff : a -> a -> p)
    (changes : (a, p, k) change list) : (a, p, k) change list =
  let rec aux = function
    | `Removed old :: `Added new_ :: rest ->
      `Modified (diff old new_) :: aux rest
    | x :: rest -> x :: aux rest
    | [] -> []
  in
  aux changes


(** Convenience function combining diff_list with merge_adjacent_changes.

    This provides replacement detection for equality-based diffing by first
    computing the Myers diff, then merging adjacent Removed+Added pairs.

    Note: This may produce different results than diff_list for the same input,
    as adjacent insert+delete pairs are collapsed into modifications.

    Note: [diff_list_generic] may choose any equally-minimal alignment, so with
    duplicate elements the exact pairs collapsed into a [`Modified] can vary
    between equally-minimal paths. The guaranteed contract: every old element
    appears exactly once in a [`Removed] or a [`Modified] oldval, every new
    element appears exactly once in an [`Added] or a [`Modified] newval, and
    the underlying edit distance is minimal.
*)
let diff_list_merged (type k)
    (module EQ : DIFFABLE_EQ)
    (old_list : EQ.t list) (new_list : EQ.t list) : (EQ.t, EQ.Patch.t, k) change list =
  diff_list (module EQ) old_list new_list
  |> merge_adjacent_changes ~diff:EQ.diff


(** Filter out Unchanged entries from a change list.

    This removes `Unchanged` entries as well as `Modified` entries where
    the patch is empty (no actual changes).

    @param P A PATCH module for the patch type
    @param changes The change list to filter
    @return Change list with Unchanged entries removed
*)
let filter_changes (module P : PATCH)
    (changes : (_, P.t) structured_change list) : (_, P.t) structured_change list =
  List.filter (fun c -> not (is_unchanged_change (module P) c)) changes

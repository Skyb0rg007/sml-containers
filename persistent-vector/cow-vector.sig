(* Copy-on-Write Vector
 *
 * Immutable vectors that support transient mutable updates.
 *
 * The intended use-case is to implement transient data structures.
 * For immutable vectors, simply use the unowned API.
 * To create a transient data structure, allocate a new owner,
 * then use that owner token in all subsequent function calls.
 * Extracting the immutable data structure at the end is a no-op;
 * throwing away the ownership token prevents any further mutation.
 * 
 * You should construct unowned vectors (owner is `noone`) when possible.
 * When the owner is `noone`, no extra memory is allocated.
 * Otherwise, all newly constructed vectors allocate `maxLen` slots.
 *)

signature COW_VECTOR =
sig

(* An ownership token.
 * Each token corresponds with the allowed mutability lifetime of a vector.
 * If a vector operation is supplied the same lifetime as it was created with,
 * then the operation is performed in-place.
 * Otherwise, the vector is copied.
 * The special owner `noone` indicates that there is no ownership,
 * and all mutable operations are disabled.
 *)
type owner

type 'a vector

(* Ownerships *)
val noone: owner
val newOwner: unit -> owner

(*** Operations on vectors ***)
(* These match the signatures and behaviors of the Vector counterparts *)
val maxLen: int
val length: 'a vector -> int
val sub: 'a vector * int -> 'a
val toList: 'a vector -> 'a list
val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val foldl: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val foldr: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val appi: (int * 'a -> unit) -> 'a vector -> unit
val app: ('a -> unit) -> 'a vector -> unit
val all: ('a -> bool) -> 'a vector -> bool
val exists: ('a -> bool) -> 'a vector -> bool
val collate: ('a * 'b -> order) -> 'a vector * 'b vector -> order

(* Returns true if the vectors refer to the same object. *)
val ptrEqual: 'a vector * 'b vector -> bool

(*** Constructing vectors ***)

(* `singleton (e, x)` create a vector with the single element `x`.
 * The resulting vector is owned by `e`. *)
val singleton: owner * 'a -> 'a vector

(* `fromList (e, xs)` creates a vector from the list `xs`.
 * The resulting vector is owned by `e`.
 * Raises `Domain` if the provided list exceeds `maxLen` elements. *)
val fromList: owner * 'a list -> 'a vector

(* `update (e, v, i, x)` returns a copy of `v` with the element at index `i` set to `x`.
 * The resulting vector is owned by `e`.
 * Raises `Subscript` if `i < 0` or `i >= length v`. *)
val update: owner * 'a vector * int * 'a -> 'a vector

(* `snoc (e, v, x)` returns a copy of `v` with the element `x` added to the end.
 * The resulting vector is owned by `e`.
 * Raises `Domain` if this would cause the vector to exceed `maxLen` elements. *)
val snoc: owner * 'a vector * 'a -> 'a vector

(* `take (e, v, i)` returns the first `i` elements of vector `v`.
 * The resulting vector is owned by `e`.
 * Raises `Subscript` if `i < 0` or `i > length v`. *)
val take: owner * 'a vector * int -> 'a vector

(* `drop (e, v, i)` returns the remaining elements of vector `v` after dropping the first `i`.
 * The resulting vector is owned by `e`.
 * Raises `Subscript` if `i < 0` or `i > length v`. *)
val drop: owner * 'a vector * int -> 'a vector

(* `append (e, v1, v2)` returns a vector which is the concatenation of `v1` and `v2`.
 * The resulting vector is owned by `e`.
 * Note: Both vector arguments are considered to be consumed by this operation.
 * Ie. if `e <> noone`, then the states of `v1` and `v2` are undefined.
 * Raises `Domain` if the resulting vector would exceed `maxLen` elements. *)
val append: owner * 'a vector * 'a vector -> 'a vector

(* Debugging *)
val check: 'a vector -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)

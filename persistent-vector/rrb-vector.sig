
signature RRB_VECTOR =
sig

type 'a vector

(* The empty vector. O(1). *)
val empty: 'a vector

(* Create a vector with one element. O(1). *)
val singleton: 'a -> 'a vector

(* Append an element. O(log(n)). *)
val snoc: 'a vector * 'a -> 'a vector

(* Access the element at a given position. O(log(n)). *)
val sub: 'a vector * int -> 'a

(* Build a vector from a list *)
val fromList: 'a list -> 'a vector

structure Transient:
   sig
      type 'a transient

      (* Create a transient vector from an immutable one. O(1). *)
      val transient: 'a vector -> 'a transient

      (* Convert a transient vector into a persistent one.
       * This resets the transient vector's mutability, so it's safe to re-use.
       * O(1). *)
      val persistent: 'a transient -> 'a vector

      (* Create an empty transient. Equivalent to `transient empty`. O(1). *)
      val empty: unit -> 'a transient

      (* Create a transient from a single element.
       * This preallocates 32 elements, so it's more efficient than `transient (singleton x)`
       * O(1). *)
      val singleton: 'a -> 'a transient

      (* Identical to the normal subscript *)
      val sub: 'a transient * int -> 'a

      (* Mutable versions of the above, which perform the updates in-place. *)
      val snoc: 'a transient * 'a -> unit
   end

end

(* vim: set tw=0 ts=3 sw=3: *)


signature ARRAY_EXTRA =
sig

(* `alloc n`
 * Creates an array of length `n`, with elements uninitialized.
 * It is an error to call `sub` on an uninitialized index.
 *)
val alloc: int -> 'a array

(* `uninit (arr, i)`
 * Sets the index `i` in the array `arr` to an uninitialized value.
 * 
 * It is an error to call `sub` on an uninitialized index.
 *)
val uninit: 'a array * int -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)

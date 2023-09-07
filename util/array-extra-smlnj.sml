
structure ArrayEx: ARRAY_EXTRA =
struct

fun alloc n = Array.array (n, Unsafe.cast ())

fun uninit (arr, i) = Array.update (arr, i, Unsafe.cast ())

end

(* vim: set tw=0 ts=3 sw=3: *)

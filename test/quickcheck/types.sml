
structure Types =
struct

datatype 'a generator = Gen of int * SplitMix.t -> 'a

datatype 'a observer = Obs of 'a * int * Word64.word -> Word64.word

fun observe (Obs obs, x, size, seed) = obs (x, size, seed)

fun generate (Gen gen, size, rng) = gen (size, rng)

end

(* vim: set tw=0 ts=3 sw=3: *)

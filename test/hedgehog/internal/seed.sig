
signature SEED =
sig

type t

val new: unit -> t
val fromWord64: Word64.word -> t
val split: t -> t * t

val nextWord64: t -> Word64.word * t
val nextWord32: t -> Word32.word * t
val nextIntInf: IntInf.int * IntInf.int -> t -> IntInf.int * t
val nextReal: t -> real * t

end

(* vim: set tw=0 ts=3 sw=3: *)

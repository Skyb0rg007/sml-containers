
signature SPLITMIX =
sig

type t

(* Creation *)
val new: unit -> t
val fromSeed: Word64.word -> t
val fromSeedGamma: Word64.word * Word64.word -> t

(* Copy the internal state. Both copies will produce the same numbers *)
val copy: t -> t

(* Split the state. The current and new states should not be correlated *)
val split: t -> t

(* Introduce some salted randomness into the state *)
val perturb: t * Word64.word -> unit

(* Random number generation *)
val word64: t -> Word64.word
val word64Upto: t -> Word64.word -> Word64.word
val intInf: t -> IntInf.int * IntInf.int -> IntInf.int

end

(* vim: set tw=0 ts=3 sw=3: *)

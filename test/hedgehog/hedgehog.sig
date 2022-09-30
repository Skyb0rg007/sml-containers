
signature HEDGEHOG =
sig

structure Gen: GEN
structure Range: RANGE
structure Test: TEST

type property

val property: unit Test.t -> property
val check: property -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)


signature TEST =
sig

include MONAD

val success: unit t
val fail: string -> 'a t
val failure: 'a t

val forallWith: ('a -> string) -> 'a Gen.t -> 'a t
val discard: 'a t
val annotate: string -> unit t
val footnote: string -> unit t

end

(* vim: set tw=0 ts=3 sw=3: *)

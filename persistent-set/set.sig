
signature SET =
sig

type item
type set

val empty: set
val singleton: item -> set
val fromList: item list -> set

val insert: set * item -> set

val delete: set * item -> set

val member: set * item -> bool

val size: set -> int

val isEmpty: set -> bool

val union: set * set -> set
val difference: set * set -> set
val intersection: set * set -> set
val disjoint: set * set -> bool
val foldl: (item * 'a -> 'a) -> 'a -> set -> 'a
val foldr: (item * 'a -> 'a) -> 'a -> set -> 'a
val app: (item -> unit) -> set -> unit
val all: (item -> bool) -> set -> bool
val exists: (item -> bool) -> set -> bool

val toList: set -> item list

val filter: (item -> bool) -> set -> set
val partition: (item -> bool) -> set -> set * set
val isSubmap: set * set -> bool
val isProperSubmap: set * set -> bool
val equals: set * set -> bool
val compare: set * set -> order

end

(* vim: set tw=0 ts=3 sw=3: *)


signature GENERATOR =
sig

type 'a t
type 'a observer

val generate: 'a t * int * SplitMix.t -> 'a
val sample: 'a t -> 'a list
val create: (int * SplitMix.t -> 'a) -> 'a t

(* Monadic interface *)
val map: ('a -> 'b) -> 'a t -> 'b t
val pure: 'a -> 'a t
val apply: ('a -> 'b) t * 'a t -> 'b t
val bind: 'a t * ('a -> 'b t) -> 'b t
val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
val map3: ('a * 'b * 'c -> 'd) -> 'a t * 'b t * 'c t -> 'd t
val join: 'a t t -> 'a t
val fix: ('a t -> 'a t) -> 'a t

(* Filtering results *)
val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
val filter: ('a -> bool) -> 'a t -> 'a t

(* Size parameter *)
val getSize: int t
val resize: int * 'a t -> 'a t
val sized: (int -> 'a t) -> 'a t
val scale: (int -> int) -> 'a t -> 'a t

(* Combinators *)
val sequence: 'a t list -> 'a list t
val sequence_: 'a t list -> unit t
val oneof: 'a t list -> 'a t
val frequency: (int * 'a t) list -> 'a t
val elements: 'a list -> 'a t
val listOfSize: int * 'a t -> 'a list t
val vectorOfSize: int * 'a t -> 'a vector t
val listOf: 'a t -> 'a list t
val vectorOf: 'a t -> 'a vector t
val func: 'a observer * 'b t -> ('a -> 'b) t

(* Choosing bounded integers *)
val chooseInt: int * int -> int t
val chooseInt32: Int32.int * Int32.int -> Int32.int t
val chooseInt64: Int64.int * Int64.int -> Int64.int t
val chooseWord: word * word -> word t
val chooseWord32: Word32.word * Word32.word -> Word32.word t
val chooseWord64: Word64.word * Word64.word -> Word64.word t
val chooseIntInf: IntInf.int * IntInf.int -> IntInf.int t

(* Basic types *)
val unit: unit t
val bool: bool t
val char: char t
val int: int t

end

(* vim: set tw=0 ts=3 sw=3: *)

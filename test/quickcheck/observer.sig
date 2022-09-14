
signature OBSERVER =
sig

type 'a t
type 'a generator

val observe: 'a t * 'a * int * Word64.word -> Word64.word
val create: ('a * int * Word64.word -> Word64.word) -> 'a t

val contramap: ('b -> 'a) -> 'a t -> 'b t
val fix: ('a t -> 'a t) -> 'a t

val unit: unit t
val bool: bool t
val char: char t
val int: int t
val int32: Int32.int t
val int64: Int64.int t
val word: word t
val word8: Word8.word t
val word32: Word32.word t
val word64: Word64.word t
val pair: 'a t * 'b t -> ('a * 'b) t
val either: 'a t * 'b t -> ('a, 'b) Either.either t
val option: 'a t -> 'a option t
val list: 'a t -> 'a list t
val vector: 'a t -> 'a vector t
val func: 'a generator * 'b t -> ('a -> 'b) t

end

(* vim: set tw=0 ts=3 sw=3: *)

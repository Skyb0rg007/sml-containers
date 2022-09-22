
signature GEN =
sig

type 'a t

(** Sampling **)
val sample: 'a t -> 'a
val print: string t -> unit
val printTree: string t -> unit

(** Shrinking **)
val shrink: ('a -> 'a Seq.t) -> 'a t -> 'a t
val prune: 'a t -> 'a t

(** Size **)
val small: 'a t -> 'a t
val scale: (int -> int) -> 'a t -> 'a t
val resize: int -> 'a t -> 'a t
val sized: (int -> 'a t) -> 'a t

(** Monadic **)
val map: ('a -> 'b) -> 'a t -> 'b t
val pure: 'a -> 'a t
val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
val ap: ('a -> 'b) t * 'a t -> 'b t
val bind: 'a t -> ('a -> 'b t) -> 'b t
val join: 'a t t -> 'a t
val mapM: ('a -> 'b t) -> 'a list -> 'b list t
val sequence: 'a t list -> 'a list t
val fix: ('a t -> 'a t) -> 'a t

(** Integral **)
val int: int Range.t -> int t
val int32: Int32.int Range.t -> Int32.int t
val int64: Int64.int Range.t -> Int64.int t
val word: word Range.t -> word t
val word8: Word8.word Range.t -> Word8.word t
val word32: Word32.word Range.t -> Word32.word t
val word64: Word64.word Range.t -> Word64.word t
val intInf: IntInf.int Range.t -> IntInf.int t
val real: real Range.t -> real t
val bool: bool t
val int_: int Range.t -> int t
val int32_: Int32.int Range.t -> Int32.int t
val int64_: Int64.int Range.t -> Int64.int t
val word_: word Range.t -> word t
val word8_: Word8.word Range.t -> Word8.word t
val word32_: Word32.word Range.t -> Word32.word t
val word64_: Word64.word Range.t -> Word64.word t
val intInf_: IntInf.int Range.t -> IntInf.int t
val real_: real Range.t -> real t
val bool_: bool t

(** Characters **)
val binDigit: char t
val octDigit: char t
val digit: char t
val hexDigit: char t
val lowerChar: char t
val upperChar: char t
val alphaChar: char t
val alphaNumChar: char t
val asciiChar: char t   (* #"\000" - #"\127" *)
val latin1Char: char t  (* #"\000" - #"\255" *)
val unicodeChar: word t (* Doesn't include Surrogate or Noncharacter codepoints *)

(** Strings **)
(* val string: int Range.t -> char t -> string t *)
(* val utf8String: int Range.t -> string t *)

(** Choice **)
val element: 'a list -> 'a t
val element_: 'a list -> 'a t
val choice: 'a t list -> 'a t
val frequency: (int * 'a t) list -> 'a t

(** Conditional **)
val or: 'a t * 'a t -> 'a t
val discard: 'a t
val ensure: ('a -> bool) -> 'a t -> 'a t
val filter: ('a -> bool) -> 'a t -> 'a t
val mapPartial: ('a -> 'b option) -> 'a t -> 'b t

(** Collections **)
(* val option: 'a t -> 'a option t *)
(* val either: 'a t * 'b t -> ('a, 'b) Either.either t *)
(* val either_: 'a t * 'b t -> ('a, 'b) Either.either t *)
val list: int Range.t -> 'a t -> 'a list t
(* val seq: 'a t -> 'a Seq.t t *)

(** Subterms **)
val recursive: ('a t list -> 'a t) -> 'a t list -> 'a t list -> 'a t
val subterm: 'a t -> ('a -> 'a) -> 'a t
val subtermM: 'a t -> ('a -> 'a t) -> 'a t
val subterm2: 'a t -> 'a t -> ('a * 'a -> 'a) -> 'a t
val subtermM2: 'a t -> 'a t -> ('a * 'a -> 'a t) -> 'a t
val subterm3: 'a t -> 'a t -> 'a t -> ('a * 'a * 'a -> 'a) -> 'a t
val subtermM3: 'a t -> 'a t -> 'a t -> ('a * 'a * 'a -> 'a t) -> 'a t

(** Combinations and Permutations **)
(* val subsequence: 'a list -> 'a list t *)
(* val shuffle: 'a list -> 'a list t *)

end

(* vim: set tw=0 ts=3 sw=3: *)

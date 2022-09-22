
signature RANGED_TYPE =
   sig
      type range
      type t

      (* A range from the minimum value to the maximum value; origin 0
       * linear in the size parameter *)
      val linear: range

      (* A linear range with custom bounds *)
      val linearFrom: {origin: t, lo: t, hi: t} -> range

      (* A range from the minimum value to the maximum value; origin 0
       * exponential in the size parameter *)
      val exponential: range

      (* An exponential range with custom bounds *)
      val exponentialFrom: {origin: t, lo: t, hi: t} -> range
   end

signature RANGE =
   sig
      type 'a t

      (* The origin point; shrinking will try to approach this value *)
      val origin: 'a t -> 'a

      (* Calculate the lower and upper bounds for a given size *)
      val bounds: 'a t * int -> 'a * 'a
      val lowerBound: 'a t * int -> 'a
      val upperBound: 'a t * int -> 'a

      (* Construct a new range from an old one *)
      val map: ('a -> 'b) -> 'a t -> 'b t

      (* A range which always has one value *)
      val singleton: 'a -> 'a t

      (* Ranges which don't depend on the size parameter *)
      val constant: {lo: 'a, hi: 'a} -> 'a t
      val constantFrom: {origin: 'a, lo: 'a, hi: 'a} -> 'a t

      structure Int:    RANGED_TYPE where type t = int         and type range = int t
      structure Int32:  RANGED_TYPE where type t = Int32.int   and type range = Int32.int t
      structure Int64:  RANGED_TYPE where type t = Int64.int   and type range = Int64.int t
      structure IntInf: RANGED_TYPE where type t = IntInf.int  and type range = IntInf.int t
      structure Word:   RANGED_TYPE where type t = word        and type range = word t
      structure Word8:  RANGED_TYPE where type t = Word8.word  and type range = Word8.word t
      structure Word32: RANGED_TYPE where type t = Word32.word and type range = Word32.word t
      structure Word64: RANGED_TYPE where type t = Word64.word and type range = Word64.word t
      structure Real:   RANGED_TYPE where type t = real        and type range = real t
      structure Char:   RANGED_TYPE where type t = char        and type range = char t
   end

(* vim: set tw=0 ts=3 sw=3: *)

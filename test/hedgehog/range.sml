
structure Range0 =
   struct
      datatype 'a t = T of 'a * (int -> 'a * 'a)

      fun origin (T (x, _)) = x

      fun bounds (T (_, f), sz) = f sz
      fun lowerBound (T (_, f), sz) = #1 (f sz)
      fun upperBound (T (_, f), sz) = #2 (f sz)

      fun map f (T (x, g)) =
         T (f x, fn sz => let val (a, b) = g sz in (f a, f b) end)

      fun singleton x = T (x, fn _ => (x, x))

      fun constant {lo, hi} = T (lo, fn _ => (lo, hi))

      fun constantFrom {origin, lo, hi} = T (origin, fn _ => (lo, hi))
   end

signature INTEGRAL =
   sig
      type t

      val min: t * t -> t
      val max: t * t -> t
      val minBound: t
      val maxBound: t
      val > : t * t -> bool
      val toLarge: t -> LargeInt.int
      val fromLarge: LargeInt.int -> t
      val fromInt: int -> t
   end

functor RangedIntegral(I: INTEGRAL): RANGED_TYPE =
   struct
      type t = I.t
      type range = t Range0.t

      fun linearFrom {origin, lo, hi} =
         if I.> (lo, hi)
            then raise Fail "linearFrom: 'lo' must be smaller than 'hi'"
         else Range0.T (origin, fn size =>
            let
               val size = LargeInt.fromInt (Int.max (0, Int.min (99, size)))
               val origin = I.toLarge origin

               fun scaleLinear n =
                  let
                     val n = I.toLarge n
                     val sign = LargeInt.sign (n - origin)
                     val rng = n - origin + LargeInt.fromInt sign
                     val diff = LargeInt.quot (rng * size, 100)
                  in
                     I.fromLarge (origin + diff)
                  end
            in
               (I.max (lo, I.min (hi, scaleLinear lo)),
                I.max (lo, I.min (hi, scaleLinear hi)))
            end)

      fun exponentialFrom {origin, lo, hi} =
         if I.> (lo, hi)
            then raise Fail "exponentialFrom: 'lo' must be smaller than 'hi'"
         else Range0.T (origin, fn size =>
            let
               val size = Int.max (0, Int.min (99, size))
               val size = Real.fromInt size
               val origin = Real.fromLargeInt (I.toLarge origin)

               fun clamp n =
                  LargeInt.max (I.toLarge lo, LargeInt.min (I.toLarge hi, n))

               fun scaleExponential n =
                  let
                     val n = Real.fromLargeInt (I.toLarge n)
                     val sign = Real.fromInt (Real.sign (n - origin))
                     val diff = (Math.pow (Real.abs (n - origin) + 1.0, size / 99.0) - 1.0) * sign
                  in
                     clamp (Real.toLargeInt IEEEReal.TO_ZERO (origin + diff))
                  end
            in
               (I.fromLarge (scaleExponential lo),
                I.fromLarge (scaleExponential hi))
            end)

      val def =
         {origin = I.fromInt 0,
          lo = I.minBound,
          hi = I.maxBound}

      val linear = linearFrom def
      val exponential = exponentialFrom def
   end

structure RangedReal: RANGED_TYPE =
   struct
      type t = real
      type range = t Range0.t

      fun linearFrom {origin, lo, hi} =
         if lo > hi
            then raise Fail "linearFrom: 'lo' must be smaller than 'hi'"
         else Range0.T (origin, fn size =>
            let
               val size = Int.max (0, Int.min (99, size))

               fun scaleLinear n =
                  let
                     val diff = (n - origin) * (Real.fromInt size / 99.0)
                  in
                     origin + diff
                  end
            in
               (Real.max (lo, Real.min (hi, scaleLinear lo)),
                Real.max (lo, Real.min (hi, scaleLinear hi)))
            end)

      fun exponentialFrom {origin, lo, hi} =
         if lo > hi
            then raise Fail "exponentialFrom: 'lo' must be smaller than 'hi'"
         else Range0.T (origin, fn size =>
            let
               val size = Int.max (0, Int.min (99, size))

               fun scaleExponential n =
                  let
                     val size = Real.fromInt size / 99.0
                     val diff =
                        (Math.pow (Real.abs (n - origin) + 1.0, size) - 1.0)
                         * Real.fromInt (Real.sign (n - origin))
                  in
                     origin + diff
                  end
            in
               (Real.max (lo, Real.min (hi, scaleExponential lo)),
                Real.max (lo, Real.min (hi, scaleExponential hi)))
            end)

      val def =
         {origin = 0.0,
          lo = ~Real.maxFinite,
          hi = Real.maxFinite}

      val linear = linearFrom def
      val exponential = exponentialFrom def
   end

functor IntegralInteger(I: INTEGER): INTEGRAL =
   struct
      open I
      type t = int

      val minBound =
         case minInt of
            SOME lo => lo
          | NONE => fromLarge ~9223372036854775808

      val maxBound =
         case maxInt of
            SOME hi => hi
          | NONE => fromLarge 9223372036854775807
   end

functor IntegralWord(W: WORD): INTEGRAL =
   struct
      open W
      type t = word

      val minBound = fromInt 0
      val maxBound = notb (fromInt 0)
      val toLarge = toLargeInt
      val fromLarge = fromLargeInt
   end

structure Range: RANGE =
   struct
      open Range0

      structure Int = RangedIntegral(IntegralInteger(Int))
      structure Int32 = RangedIntegral(IntegralInteger(Int32))
      structure Int64 = RangedIntegral(IntegralInteger(Int64))
      structure IntInf = RangedIntegral(IntegralInteger(IntInf))
      structure Word = RangedIntegral(IntegralWord(Word))
      structure Word8 = RangedIntegral(IntegralWord(Word8))
      structure Word32 = RangedIntegral(IntegralWord(Word32))
      structure Word64 = RangedIntegral(IntegralWord(Word64))
      structure Real = RangedReal
      structure Char =
         struct
            type range = char t
            type t = char

            fun arg {origin, lo, hi} =
               {origin = Byte.charToByte origin,
                lo = Byte.charToByte lo,
                hi = Byte.charToByte hi}

            val linear = map Byte.byteToChar Word8.linear
            fun linearFrom r = map Byte.byteToChar (Word8.linearFrom (arg r))
            val exponential = map Byte.byteToChar Word8.exponential
            fun exponentialFrom r = map Byte.byteToChar (Word8.exponentialFrom (arg r))
         end
   end

(* vim: set tw=0 ts=3 sw=3: *)

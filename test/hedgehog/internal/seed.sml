
structure Seed =
struct

datatype t = T of {gamma: Word64.word, value: Word64.word}

(** Bitwise helpers **)

fun popCount 0wxffffffffffffffff = 64
  | popCount w =
   let
      open Word64
      infix >> andb

      val w = w - ((w >> 0w1) andb 0wx5555555555555555)
      val w = (w andb 0wx3333333333333333) + ((w >> 0w2) andb 0wx3333333333333333)
      val w = ((w + (w >> 0w4)) andb 0wx0f0f0f0f0f0f0f0f) * 0wx0101010101010101
   in
      Word64.toInt (w >> 0w56)
   end

fun leadingZeros 0w0 = 64
  | leadingZeros w =
   let
      val op andb = Word64.andb
      val op << = Word64.<<
      infix andb <<

      val (w, n) =
         if (w andb 0wxffffffff00000000) = 0w0
            then (w << 0w32, 32)
         else (w, 0)
      val (w, n) =
         if (w andb 0wxffff000000000000) = 0w0
            then (w << 0w16, n + 16)
         else (w, n)
      val (w, n) =
         if (w andb 0wxff00000000000000) = 0w0
            then (w << 0w8, n + 8)
         else (w, n)
      val (w, n) =
         if (w andb 0wxf000000000000000) = 0w0
            then (w << 0w4, n + 4)
         else (w, n)
      val (w, n) =
         if (w andb 0wxc000000000000000) = 0w0
            then (w << 0w2, n + 2)
         else (w, n)
      val n =
         if (w andb 0wx8000000000000000) = 0w0
            then n + 1
         else n
   in
      n
   end

fun mix64 w =
   let
      open Word64
      infix >> xorb

      val w = (w xorb (w >> 0w33)) * 0wxff51afd7ed558ccd
      val w = (w xorb (w >> 0w33)) * 0wxc4ceb9fe1a85ec53
   in
      w xorb (w >> 0w33)
   end

fun mix32 w =
   let
      open Word64
      infix >> xorb

      val w = (w xorb (w >> 0w33)) * 0wxff51afd7ed558ccd
      val w = (w xorb (w >> 0w33)) * 0wxc4ceb9fe1a85ec53
   in
      Word32.fromLarge (w >> 0w32)
   end

fun mixGamma w =
   let
      open Word64
      infix >> xorb orb

      val w = (w xorb (w >> 0w30)) * 0wxbf58476d1ce4e5b9
      val w = (w xorb (w >> 0w27)) * 0wx94d049bb133111eb
      val w = (w xorb (w >> 0w31))
      val w = w orb 0w1
      val n = popCount (w xorb (w >> 0w1))
   in
      if Int.< (n, 24)
         then w xorb 0wxaaaaaaaaaaaaaaaa
      else w
   end

(** Exports **)

fun fromWord64 w =
   let
      val goldenGamma = 0wx9e3779b97f4a7c15
   in
      T {gamma = mixGamma (w + goldenGamma), value = mix64 w}
   end

fun new () =
   let
      val t = Time.toNanoseconds (Time.now ())
   in
      fromWord64 (Word64.fromLargeInt t)
   end

fun split (T {gamma, value}) =
   let
      val v = gamma + value
      val g = gamma + v
   in
      (T {gamma = gamma, value = g},
       T {gamma = mixGamma g, value = mix64 v})
   end

fun nextWord64 (T {gamma, value}) =
   let
      val v = gamma + value
   in
      (mix64 v, T {gamma = gamma, value = v})
   end

fun nextWord32 (T {gamma, value}) =
   let
      val v = gamma + value
   in
      (mix32 v, T {gamma = gamma, value = v})
   end

(* Generates a random word less than `range`
 * Algorithm:
 *   1. Generate a 64-bit word
 *   2. Mask out bits until it has the same number of bits as `range`
 *   3. If less than range, return it. Otherwise repeat step 1. *)
fun bitmaskWithRejection (range, gen) =
   let
      val mask = Word64.>> (Word64.notb 0w0, Word.fromInt (leadingZeros (Word64.orb (range, 0w1))))

      fun go gen =
         let
            val (w, gen) = nextWord64 gen
            val w = Word64.andb (w, mask)
         in
            if w < range
               then (w, gen)
            else go gen
         end
   in
      go gen
   end

(* Calculate the number of 64-bit words long the IntInt.int is *)
fun intInfWordSize n =
   let
      fun go (0, acc) = acc
        | go (i, acc) = go (IntInf.~>> (i, Word.fromInt Word64.wordSize), acc + 1)
   in
      go (n, 0)
   end

(* Generate a given number of 64-bit words, joining them together to create
 * a long IntInf.int value *)
fun nextWord64s n gen =
   let
      fun go (0, acc, gen) = (acc, gen)
        | go (i, acc, gen) =
         let
            val (w, gen) = nextWord64 gen
            val acc' =
               IntInf.orb (IntInf.<< (acc, Word.fromInt Word64.wordSize),
                           Word64.toLargeInt w)
         in
            go (i - 1, acc', gen)
         end
   in
      go (n, 0, gen)
   end

(* Generate a random number in the inclusive range `[0, range]` *)
fun nextIntInf' (range, gen) =
   let
      val n = intInfWordSize range
      val k = Word.fromInt (Word64.wordSize * n)
      val twoToK = IntInf.<< (1, k)
      val modTwoToKMask = twoToK - 1
      val t = IntInf.rem (twoToK - range, range)

      fun go gen =
         let
            val (x, gen) = nextWord64s n gen
            val m = x * range
            val l = IntInf.andb (m, modTwoToKMask)
         in
            if l < t
               then go gen
            else (IntInf.~>> (m, k), gen)
         end
   in
      go gen
   end

fun nextIntInf (lo, hi) gen =
   case IntInf.compare (lo, hi) of
      GREATER => nextIntInf (hi, lo) gen
    | EQUAL => (lo, gen)
    | LESS =>
         let
            val limit = hi - lo
            val (bounded, gen) =
               if limit < Word64.toLargeInt (Word64.notb 0w0)
                  then
                     (* Optimized algorithm if limit fits in 64-bit word *)
                     let
                        val n = Word64.fromLargeInt limit
                        val (w, gen) = bitmaskWithRejection (n + 0w1, gen)
                     in
                        (Word64.toLargeInt w, gen)
                     end
               else nextIntInf' (limit + 1, gen)
         in
            (lo + bounded, gen)
         end

val w64ToReal = Real.fromLargeInt o Word64.toLargeInt

(* Generate a real between 0 and 1 *)
fun nextReal01 gen =
   let
      val (w, gen) = nextWord64 gen
   in
      (w64ToReal w / w64ToReal (Word64.notb 0w0), gen)
   end

fun nextReal (lo, hi) gen =
   if Real.== (lo, hi)
      then (lo, gen)
   else if Real.isFinite lo andalso Real.isFinite hi
      then
         let
            val (x, gen) = nextReal01 gen
         in
            (x * lo + (1.0 - x) * hi, gen)
         end
   else
      (* This happens to work properly when lo and hi are ±inf or NaN *)
      (lo + hi, gen)

end

(* vim: set tw=0 ts=3 sw=3: *)

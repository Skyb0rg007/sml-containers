
structure SplitMix: SPLITMIX =
struct

datatype t = T of {
   seed: Word64.word ref,
   gamma: Word64.word
}

fun fromSeed w = T {
      seed = ref w,
      gamma = 0wx9e3779b97f4a7c15
   }

fun copy (T {seed, gamma}) = T {seed = ref (!seed), gamma = gamma}

fun mix_bits (z, n) =
   Word64.xorb (z, Word64.>> (z, n))

fun mix64 z =
   let
      val z = mix_bits (z, 0w33) * 0wxff51afd7ed558ccd
      val z = mix_bits (z, 0w33) * 0wxc4ceb9fe1a85ec53
   in
      mix_bits (z, 0w33)
   end

fun mix64_variant13 z =
   let
      val z = mix_bits (z, 0w30) * 0wxbf58476d1ce4e5b9
      val z = mix_bits (z, 0w30) * 0wx94d049bb133111eb
   in
      mix_bits (z, 0w31)
   end

fun popCount64 0wxffffffffffffffff = 64
  | popCount64 w =
   let
      open Word64
      infix >> andb

      val w = w - ((w >> 0w1) andb 0wx5555555555555555)
      val w = (w andb 0wx3333333333333333) + ((w >> 0w2) andb 0wx3333333333333333)
      val w = ((w + (w >> 0w4)) andb 0wx0f0f0f0f0f0f0f0f) * 0wx0101010101010101
   in
      toInt (w >> 0w56)
   end

fun mix_odd_gamma z =
   let
      val z = Word64.orb (mix64_variant13 z, 0w1)
      val n = popCount64 (Word64.xorb (z, Word64.>> (z, 0w1)))
   in
      if n < 24
         then Word64.xorb (z, 0wxaaaaaaaaaaaaaaaa)
      else z
   end

fun perturb (T {seed, ...}, salt) = seed := !seed + mix64 salt

fun next_seed (T {seed, gamma}) =
   let
      val next = !seed + gamma
   in
      seed := next
      ; next
   end

fun fromSeedGamma (seed, gamma) = T {
      seed = ref (mix64 seed),
      gamma = mix_odd_gamma gamma
   }

fun split t = 
   let
      val seed = next_seed t
      val gamma = next_seed t
   in
      fromSeedGamma (seed, gamma)
   end

fun nextWord64 t = mix64 (next_seed t)

local
   fun go (t, rem_max) =
      let
         val draw = nextWord64 t
         val draw_max = 0wxffffffffffffffff
         val remainder = Word64.mod (draw, rem_max + 0w1)
      in
         if draw - remainder <= draw_max - rem_max
            then remainder
         else go (t, rem_max)
      end
in
   fun word64 t (0w0, 0wxffffffffffffffff) = nextWord64 t
     | word64 t (lo, hi) =
      if lo > hi
         then raise Fail "SplitMix.word64: lo must be smaller than hi"
      else go (t, hi - lo) + lo
end

fun int t (lo, hi) =
   let
      val w = word64 t (Word64.fromInt lo, Word64.fromInt hi)
      val mask =
         Word64.<< (0w1, Word.fromInt (Option.getOpt (Int.precision, 64))) - 0w1
   in
      Word64.toIntX (Word64.andb (w, mask))
   end

local
   fun bits_to_represent k =
      let
         val k = ref k
         val n = ref 0
      in
         while (!k > 0) do (k := !k div 2; n := !n + 1)
         ; !n
      end

   fun min_rep_by 0 = 0
     | min_rep_by n = Word.toInt (Word.<< (0w1, Word.fromInt (n - 1)))

   fun max_rep_by n = Word.toInt (Word.<< (0w1, Word.fromInt n)) - 1
in
   fun logInt t (lo, hi) =
      let
         val min_bits = bits_to_represent lo
         val max_bits = bits_to_represent hi
         val bits = int t (min_bits, max_bits)
      in
         int t (Int.max (lo, min_rep_by bits), Int.min (hi, max_rep_by bits))
      end
end

structure Test =
struct
   fun test_odd_gamma () =
      let
         fun test input =
            let
               val output = mix_odd_gamma (Word64.fromLargeInt input)
            in
               if output mod 0w2 = 0w0
                  then raise Fail ("Gamma value is not odd! (input: " ^ IntInf.toString input ^ ")")
               else ()
            end

         fun go 1000001 = ()
           | go n = (test n; go (n + 1))
      in
         go ~1000000
      end

   fun remainder_unbiased () =
      let
         val draw_max = 104
         val rem_max = 9
         fun is_unbiased draw =
            let
               val remainder = IntInf.rem (draw, rem_max + 1)
            in
               draw - remainder <= draw_max - rem_max
            end

         fun for (i, j, f) =
            if i < j
               then (f i; for (i + 1, j, f))
            else ()
      in
         for (0, 99, fn i =>
            if is_unbiased i
               then ()
            else raise Fail "Is unbiased for 0-99")
         ;
         for (100, 104, fn i =>
            if not (is_unbiased i)
               then ()
            else raise Fail "Isn't unbiased for 100-104")
      end
end

end

(* vim: set tw=0 ts=3 sw=3: *)

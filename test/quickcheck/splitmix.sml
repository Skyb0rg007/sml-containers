
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

fun word64 t = mix64 (next_seed t)

fun leadingZeros 0w0 = 64
  | leadingZeros x =
   let
      val n = 0
      val (x, n) =
         if Word64.andb (x, 0wxffffffff00000000) = 0w0
            then (Word64.<< (x, 0w32), n + 32)
         else (x, n)
      val (x, n) =
         if Word64.andb (x, 0wxffff000000000000) = 0w0
            then (Word64.<< (x, 0w16), n + 16)
         else (x, n)
      val (x, n) =
         if Word64.andb (x, 0wxff00000000000000) = 0w0
            then (Word64.<< (x, 0w8), n + 8)
         else (x, n)
      val (x, n) =
         if Word64.andb (x, 0wxf000000000000000) = 0w0
            then (Word64.<< (x, 0w4), n + 4)
         else (x, n)
      val (x, n) =
         if Word64.andb (x, 0wxc000000000000000) = 0w0
            then (Word64.<< (x, 0w2), n + 2)
         else (x, n)
      val n =
         if Word64.andb (x, 0wx8000000000000000) = 0w0
            then n + 1
         else n
   in
      n
   end

fun word64Upto t range =
   let
      val mask = Word64.>> (Word64.notb 0w0, Word.fromInt (leadingZeros range))

      fun loop () =
         let
            val w = Word64.andb (word64 t, mask)
         in
            if w > range
               then loop ()
            else w
         end
   in
      loop ()
   end

local
   val two64 = IntInf.pow (2, 64)

   fun intInf' (t, range) =
      let
         (* Mask for the highest 64 bits; number of other 64-bit words *)
         val (leadMask, restDigits) =
            let
               fun go (n, x) =
                  if x < two64
                     then
                        let
                           val x = Word64.fromLargeInt x
                           val zeros = Word.fromInt (leadingZeros x)
                        in
                           (Word64.>> (Word64.notb 0w0, zeros), n)
                        end
                  else go (n + 1, IntInf.~>> (x, 0w64))
            in
               go (0, range)
            end

         fun generate () =
            let
               (* Leading word64 gets masked *)
               val lead = Word64.toLargeInt (Word64.andb (word64 t, leadMask))

               (* Other word64s get added on *)
               fun go (0, acc) = acc
                 | go (n, acc) =
                  let
                     val x = Word64.toLargeInt (word64 t)
                  in
                     go (n - 1, IntInf.<< (acc, 0w64) + x)
                  end
            in
               go (restDigits, lead)
            end

         (* Keep trying until its within the range *)
         fun loop () =
            let
               val x = generate ()
            in
               if x > range
                  then loop ()
               else x
            end
      in
         loop ()
      end
in
   fun intInf t (lo, hi) =
      case IntInf.compare (lo, hi) of
         LESS => intInf' (t, hi - lo) + lo
       | EQUAL => lo
       | GREATER => intInf' (t, lo - hi) + hi
end

fun urandom () =
   let
      val infile = BinIO.openIn "/dev/urandom"
      val bytes = BinIO.inputN (infile, 8)
      val () = BinIO.closeIn infile
   in
      Word8Vector.foldl
         (fn (byte, w) =>
            Word64.<< (w, 0w8) + Word8.toLarge byte)
         0w0
         bytes
   end

fun timeRandom () =
   Word64.fromLargeInt (Time.toNanoseconds (Time.now ()))

fun new () = fromSeed (urandom () handle Io => timeRandom ())

(* local *)
(*    fun go (t, rem_max) = *)
(*       let *)
(*          val draw = word64 t *)
(*          val draw_max = 0wxffffffffffffffff *)
(*          val remainder = Word64.mod (draw, rem_max + 0w1) *)
(*       in *)
(*          if draw - remainder <= draw_max - rem_max *)
(*             then remainder *)
(*          else go (t, rem_max) *)
(*       end *)
(* in *)
(*    fun chooseWord64 t (0w0, 0wxffffffffffffffff) = word64 t *)
(*      | chooseWord64 t (lo, hi) = *)
(*       if lo > hi *)
(*          then raise Fail "SplitMix.chooseWord64: lo must be smaller than hi" *)
(*       else go (t, hi - lo) + lo *)
(* end *)

(* fun chooseInt t (lo, hi) = *)
(*    let *)
(*       val w = chooseWord64 t (Word64.fromInt lo, Word64.fromInt hi) *)
(*          handle Overflow => raise Fail "SplitMix.chooseInt: ints >64 bits not supported" *)
(*       val mask = *)
(*          Word64.<< (0w1, Word.fromInt (Option.getOpt (Int.precision, 64))) - 0w1 *)
(*    in *)
(*       Word64.toIntX (Word64.andb (w, mask)) *)
(*    end *)

(* local *)
(*    fun bits_to_represent k = *)
(*       let *)
(*          val k = ref k *)
(*          val n = ref 0 *)
(*       in *)
(*          while (!k > 0) do (k := !k div 2; n := !n + 1) *)
(*          ; !n *)
(*       end *)

(*    fun min_rep_by 0 = 0 *)
(*      | min_rep_by n = Word.toInt (Word.<< (0w1, Word.fromInt (n - 1))) *)

(*    fun max_rep_by n = Word.toInt (Word.<< (0w1, Word.fromInt n)) - 1 *)
(* in *)
(*    fun chooseLogInt t (lo, hi) = *)
(*       let *)
(*          val min_bits = bits_to_represent lo *)
(*          val max_bits = bits_to_represent hi *)
(*          val bits = chooseInt t (min_bits, max_bits) *)
(*       in *)
(*          chooseInt t (Int.max (lo, min_rep_by bits), Int.min (hi, max_rep_by bits)) *)
(*       end *)
(* end *)

(* structure Test = *)
(* struct *)
(*    fun test_odd_gamma () = *)
(*       let *)
(*          fun test input = *)
(*             let *)
(*                val output = mix_odd_gamma (Word64.fromLargeInt input) *)
(*             in *)
(*                if output mod 0w2 = 0w0 *)
(*                   then raise Fail ("Gamma value is not odd! (input: " ^ IntInf.toString input ^ ")") *)
(*                else () *)
(*             end *)

(*          fun go 1000001 = () *)
(*            | go n = (test n; go (n + 1)) *)
(*       in *)
(*          go ~1000000 *)
(*       end *)

(*    fun remainder_unbiased () = *)
(*       let *)
(*          val draw_max = 104 *)
(*          val rem_max = 9 *)
(*          fun is_unbiased draw = *)
(*             let *)
(*                val remainder = IntInf.rem (draw, rem_max + 1) *)
(*             in *)
(*                draw - remainder <= draw_max - rem_max *)
(*             end *)

(*          fun for (i, j, f) = *)
(*             if i < j *)
(*                then (f i; for (i + 1, j, f)) *)
(*             else () *)
(*       in *)
(*          for (0, 99, fn i => *)
(*             if is_unbiased i *)
(*                then () *)
(*             else raise Fail "Is unbiased for 0-99") *)
(*          ; *)
(*          for (100, 104, fn i => *)
(*             if not (is_unbiased i) *)
(*                then () *)
(*             else raise Fail "Isn't unbiased for 100-104") *)
(*       end *)
(* end *)

end

(* vim: set tw=0 ts=3 sw=3: *)

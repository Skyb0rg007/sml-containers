
structure Generator: GENERATOR =
struct

type 'a t = 'a Types.generator
type 'a observer = 'a Types.observer

val generate = Types.generate

val create = Types.Gen

fun sample gen =
   let
      val size = 30
      val rng = SplitMix.new ()
   in
      List.tabulate (10, fn _ => generate (gen, size, rng))
   end

(** Monadic Interface **)

val getSize = create (fn (size, _) => size)

fun resize (size, gen) = create (fn (_, rng) => generate (gen, size, rng))

fun sized f = create (fn (size, rng) => generate (f size, size, rng))

fun scale f gen = sized (fn size => resize (f size, gen))

fun mapPartial f gen =
   let
      fun go (size, rng) =
         let
            val x = generate (gen, size, rng)
         in
            case f x of
               SOME y => y
             | NONE => go (size + 1, rng)
         end
   in
      create go
   end

fun filter f = mapPartial (fn x => if f x then SOME x else NONE)

fun map f gen = create (fn (size, rng) => f (generate (gen, size, rng)))

fun apply (gf, gx) =
   create (fn (size, rng) =>
      let
         val f = generate (gf, size, rng)
         val x = generate (gx, size, rng)
      in
         f x
      end)

fun bind (gen, f) =
   create (fn (size, rng) =>
      generate (f (generate (gen, size, rng)), size, rng))

fun pure x = create (fn _ => x)

fun map2 f (g1, g2) =
   create (fn (size, rng) =>
      let
         val x = generate (g1, size, rng)
         val y = generate (g2, size, rng)
      in
         f (x, y)
      end)

fun map3 f (g1, g2, g3) =
   create (fn (size, rng) =>
      let
         val x = generate (g1, size, rng)
         val y = generate (g2, size, rng)
         val z = generate (g3, size, rng)
      in
         f (x, y, z)
      end)

fun join g = bind (g, fn x => x)

fun fix f =
   let
      val r = ref (create (fn _ => raise Fail "Generate.fix: forced too early"))

      fun gen (size, rng) =
         generate (!r, size, rng)

      val res = f (create gen)
   in
      r := res
      ; res
   end

(** Integer generation **)

fun chooseWord64 (lo, hi) =
   if lo > hi
      then raise Fail "Generator.chooseWord64: crossed bounds"
   else
      create (fn (_, rng) => SplitMix.word64Upto rng (hi - lo) + lo)

fun chooseInt64 (lo, hi) =
   if lo > hi
      then raise Fail "Generator.chooseInt64: crossed bounds"
   else
      let
         val lo = Word64.fromLargeInt (Int64.toLarge lo)
         val hi = Word64.fromLargeInt (Int64.toLarge hi)
      in
         (* TODO: Test this *)
         create (fn (_, rng) =>
            Int64.fromLarge (Word64.toLargeIntX (SplitMix.word64Upto rng (hi - lo) + lo)))
      end

fun chooseIntInf (lo, hi) =
   if lo > hi
      then raise Fail "Generator.chooseIntInf: crossed bounds"
   else 
      if ~(IntInf.pow (2, 63)) <= lo andalso hi < IntInf.pow (2, 63)
         then map Int64.toLarge (chooseInt64 (Int64.fromLarge lo, Int64.fromLarge hi))
      else if 0 <= lo andalso hi < IntInf.pow (2, 64)
         then map Word64.toLargeInt (chooseWord64 (Word64.fromLargeInt lo, Word64.fromLargeInt hi))
      else
         create (fn (_, rng) => SplitMix.intInf rng (lo, hi))

fun chooseInt32 (lo, hi) =
   if lo > hi
      then raise Fail "Generator.chooseInt32: crossed bounds"
   else
      let
         val to = Int32.fromInt o Int64.toInt
         val from = Int64.fromInt o Int32.toInt
      in
         map to (chooseInt64 (from lo, from hi))
      end

fun chooseWord32 (lo, hi) =
   if lo > hi
      then raise Fail "Generator.chooseWord32: crossed bounds"
   else
      map Word32.fromLarge (chooseWord64 (Word32.toLarge lo, Word32.toLarge hi))

fun chooseWord (lo, hi) =
   if lo > hi
      then raise Fail "Generator.chooseWord: crossed bounds"
   else
      map Word.fromLarge (chooseWord64 (Word.toLarge lo, Word.toLarge hi))

val chooseInt =
   if Option.isSome Int.precision andalso Option.valOf Int.precision <= 64
      then
         fn (lo, hi) =>
            if lo > hi
               then raise Fail "Generator.chooseInt: crossed bounds"
            else
               map Int64.toInt (chooseInt64 (Int64.fromInt lo, Int64.fromInt hi))
   else
      fn (lo, hi) =>
         if lo > hi
            then raise Fail "Generator.chooseInt: crossed bounds"
         else
            map Int.fromLarge (chooseIntInf (Int.toLarge lo, Int.toLarge hi))

(** Combinators **)

fun sequence gens =
   create (fn (size, rng) =>
      List.map (fn gen => generate (gen, size, rng)) gens)

fun sequence_ gens =
   create (fn (size, rng) =>
      List.app (fn gen => ignore (gen, size, rng)) gens)

fun oneof [] = raise Fail "Generator.oneof: empty list"
  | oneof xs =
   let
      val xs = Vector.fromList xs
      val lim = Word64.fromInt (Vector.length xs - 1)
      fun gen (size, rng) =
         let
            val idx = Word64.toInt (SplitMix.word64Upto rng lim)
         in
            generate (Vector.sub (xs, idx), size, rng)
         end
   in
      create gen
   end

fun frequency [] = raise Fail "Generator.frequency: empty list"
  | frequency xs =
   if List.all (fn (k, _) => k = 0) xs
      then raise Fail "Generator.frequency: all weights are zero"
   else if List.exists (fn (k, _) => k < 0) xs
      then raise Fail "Generator.frequency: negative weight"
   else
      let
         val sum = List.foldl (fn ((k, _), n) => k + n) 0 xs

         fun pick (_, []) = raise Fail "Generator.frequency: internal error"
           | pick (n, (k, x) :: xs) =
            if n <= k
               then x
            else pick (n - 1, xs)

         fun gen (size, rng) =
            let
               val n = SplitMix.word64Upto rng (Word64.fromInt (sum - 1))
               val g = pick (Word64.toInt n + 1, xs)
            in
               generate (g, size, rng)
            end
      in
         create gen
      end

fun elements [] = raise Fail "Generator.elements: empty list"
  | elements xs =
   let
      val xs = Vector.fromList xs
      val lim = Word64.fromInt (Vector.length xs - 1)
   in
      create (fn (_, rng) =>
         let
            val n = Word64.toInt (SplitMix.word64Upto rng lim)
         in
            Vector.sub (xs, n)
         end)
   end

fun vectorOfSize (n, gen) =
   create (fn (size, rng) =>
      Vector.tabulate (n, fn _ => generate (gen, size, rng)))

fun listOfSize (n, gen) =
   create (fn (size, rng) =>
      List.tabulate (n, fn _ => generate (gen, size, rng)))

fun listOf gen =
   sized (fn n =>
   bind (chooseInt (0, n), fn k =>
   listOfSize (k, gen)))

fun vectorOf gen =
   sized (fn n =>
   bind (chooseInt (0, n), fn k =>
   vectorOfSize (k, gen)))

val unit = create (fn _ => ())
val bool = create (fn (_, rng) => SplitMix.word64Upto rng 0w1 = 0w0)
val char = map Char.chr (chooseInt (0, 255))

(* TODO: More efficient versions *)
val int =
   let
      val (min, max) =
         case (Int.minInt, Int.maxInt) of
            (SOME a, SOME b) => (a, b)
          | _ => (Int.fromLarge (~(IntInf.pow (2, 63))),
                  Int.fromLarge (IntInf.pow (2, 63)))
   in
      chooseInt (min, max)
   end

fun func (domain, range) =
   let
      fun gen (size, rng) =
         let
            (* Split the RNG, since the returned function must be pure *)
            val rng = SplitMix.split rng
         in
            fn x =>
               let
                  (* 2^64 - 59, largest 64-bit prime number *)
                  val hash = 0w18446744073709551557
                  val hash = Types.observe (domain, x, size, hash)
                  val rng = SplitMix.copy rng
                  val () = SplitMix.perturb (rng, hash)
               in
                  generate (range, size, rng)
               end
         end
   in
      create gen
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

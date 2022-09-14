
structure Generator =
struct

type 'a t = 'a Types.generator
type 'a observer = 'a Types.observer

val generate = Types.generate

val create = Types.Gen

fun sizes (min_len, max_len) =
   create (fn (size, rng) =>
      let
         val max_len = Int.min (max_len, min_len + size)
            handle Overflow => max_len

         val len = SplitMix.logInt rng (min_len, max_len)

         fun for (i, j, k) =
            if i < j
               then (k i; for (i + 1, j, k))
            else ()
      in
         if len = 0
            then []
         else
            let
               val sizes = Array.array (len, 0)
               val remaining = size - (len - min_len)

               val () =
                  for (0, remaining, fn _ =>
                     let
                        val idx = SplitMix.logInt rng (0, len - 1)
                     in
                        Array.update (sizes, idx, Array.sub (sizes, idx) + 1)
                     end)

               val () =
                  for (0, len - 1, fn i =>
                     let
                        val j = SplitMix.int rng (i, len - 1)
                        val x = Array.sub (sizes, i)
                        val y = Array.sub (sizes, j)
                     in
                        Array.update (sizes, i, y)
                        ; Array.update (sizes, j, x)
                     end)
            in
               Array.toList sizes
            end
      end)


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
      val len = Vector.length xs
      fun gen (size, rng) =
         let
            val idx = SplitMix.int rng (0, len - 1)
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
               val n = SplitMix.int rng (1, sum)
               val g = pick (n, xs)
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
      val len = Vector.length xs
   in
      create (fn (_, rng) => Vector.sub (xs, SplitMix.int rng (0, len - 1)))
   end

fun vectorOfSize (n, gen) =
   create (fn (size, rng) =>
      Vector.tabulate (n, fn _ => generate (gen, size, rng)))

fun listOfSize (n, gen) =
   create (fn (size, rng) =>
      List.tabulate (n, fn _ => generate (gen, size, rng)))

val unit = create (fn _ => ())
val bool = create (fn (_, rng) => SplitMix.word64 rng (0w0, 0w1) = 0w0)
val char = create (fn (_, rng) => Char.chr (SplitMix.int rng (0, 255)))
val int = create (fn (_, rng) => SplitMix.int rng (0, valOf Int.maxInt))

(* fun union xs = join (fromList xs) *)

(* fun int (lo, hi) = T (fn (_, rng) => SplitMix.int rng (lo, hi)) *)

(* fun fromWeightedList [] = raise Fail "Generator.fromWeightedList: empty list" *)
(*   | fromWeightedList xs = *)
(*    if List.all (fn (k, _) => k = 0) xs *)
(*       then raise Fail "Generator.fromWeightedList: all weights are zero" *)
(*    else if List.exists (fn (k, _) => k < 0) xs *)
(*       then raise Fail "Generator.fromWeightedList: negative weight" *)
(*    else *)
(*       let *)
(*          val tot = List.foldr (fn ((k, _), n) => k + n) 0 xs *)
(*          fun pick (_, []) = raise Fail "Generator.fromWeightedList: internal error" *)
(*            | pick (n, (k, x) :: xs) = *)
(*             if n <= k *)
(*                then x *)
(*             else pick (n - 1, xs) *)
(*       in *)
(*          bind (int (1, tot), fn n => pick (n, xs)) *)
(*       end *)

(* structure Observer = *)
(*    struct *)
(*       type 'a gen = 'a t *)
(*       datatype 'a t = T of 'a * int * Word64.word -> Word64.word *)

(*       fun observe (T f, x, size, seed) = f (x, size, seed) *)

(*       fun contramap f (T gen) = T (fn (x, size, seed) => gen (f x, size, seed)) *)

(*       fun fixed_point wrap = *)
(*          let *)
(*             val r = ref (fn _ => raise Fail "Observer.fixed_point: forced") *)
(*             fun go x = !r x *)
(*             val T res = wrap (T go) *)
(*          in *)
(*             r := res *)
(*             ; T res *)
(*          end *)

(*       fun hashCombine (seed, hash) = *)
(*          Word64.xorb (seed, hash + 0wx9e3779b9 + Word64.<< (seed, 0w6) + Word64.>> (seed, 0w2)) *)

(*       val unit = T (fn ((), _, seed) => seed) *)

(*       val bool = T (fn (b, _, seed) => hashCombine (seed, if b then 0w1 else 0w2)) *)

(*       val char = T (fn (c, _, seed) => hashCombine (seed, Word64.fromInt (Char.ord c))) *)

(*       val int = T (fn (n, _, seed) => hashCombine (seed, Word64.fromInt n)) *)

(*       val word64 = T (fn (w, _, seed) => hashCombine (seed, w)) *)

(*       fun pair (T oa, T ob) = *)
(*          T (fn ((a, b), size, seed) => ob (b, size, oa (a, size, seed))) *)

(*       fun either (T oa, T ob) = *)
(*          T (fn (Either.INL a, size, seed) => oa (a, size, hashCombine (seed, 0w1)) *)
(*              | (Either.INR b, size, seed) => ob (b, size, hashCombine (seed, 0w2))) *)

(*       fun option (T obs) = *)
(*          T (fn (NONE, _, seed) => hashCombine (seed, 0w1) *)
(*              | (SOME a, size, seed) => obs (a, size, hashCombine (seed, 0w2))) *)

(*       fun list (T obs) = T (fn (xs, size, seed) => *)
(*          let *)
(*             val len = List.length xs *)
(*             val szs = generate (sizes (len, len), size, SplitMix.fromSeed seed) *)
(*          in *)
(*             ListPair.foldlEq *)
(*                (fn (x, size, seed) => obs (x, size, hashCombine (seed, 0w1))) *)
(*                (hashCombine (seed, 0w0)) *)
(*                (xs, szs) *)
(*          end) *)

(*       fun func (domain, range) = T (fn (f, size, seed) => *)
(*          let *)
(*             val rng = SplitMix.fromSeed seed *)
(*             val sizes = generate (sizes (0, Option.getOpt (Int.maxInt, 4294967296)), size * 2, rng) *)
(*          in *)
(*             List.foldl *)
(*                (fn (size, seed) => *)
(*                   observe (range, f (generate (domain, size, rng)), size, seed)) *)
(*                seed *)
(*                sizes *)
(*          end) *)
(*    end *)

(* fun func (Observer.T domain, T range) = *)
(*    let *)
(*       fun gen (size, rng) = *)
(*          let *)
(*             val rng = SplitMix.split rng *)
(*          in *)
(*             fn x => *)
(*                let *)
(*                   val hash = domain (x, size, 0w0) *)
(*                   val rng = SplitMix.copy rng *)
(*                in *)
(*                   SplitMix.perturb (rng, hash) *)
(*                   ; range (size, rng) *)
(*                end *)
(*          end *)
(*    in *)
(*       T gen *)
(*    end *)


end

(* vim: set tw=0 ts=3 sw=3: *)

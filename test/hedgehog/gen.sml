
(* structure Gen: GEN = *)
structure Gen =
struct

datatype 'a t = T of int * Seed.t -> 'a Tree.t option

fun run (T gen, size, seed) = gen (size, seed)

fun sample gen =
   let
      val size = 30

      val rng = ref (Seed.new ())
      fun seed () =
         let
            val (s1, s2) = Seed.split (!rng)
         in
            rng := s1
            ; s2
         end

      fun go n =
         if n <= 0
            then raise Fail "Gen.sample: too many discards, could not generate a sample"
         else
            case run (gen, size, seed ()) of
               NONE => go (n - 1)
             | SOME t => Tree.root t
   in
      go 100
   end

fun print gen =
   let
      val size = 30
      val seed = Seed.new ()
   in
      case run (gen, size, seed) of
         NONE => TextIO.print "=== Outcome ===\n<discard>\n"
       | SOME t =>
             (TextIO.print "=== Outcome ===\n"
              ; TextIO.print (Tree.root t)
              ; TextIO.print "\n=== Shrinks ===\n"
              ; Seq.app
                   (fn child => TextIO.print (Tree.root child ^ "\n"))
                   (Tree.children t))
   end

fun printTree gen =
   let
      val size = 30
      val seed = Seed.new ()
   in
      case run (gen, size, seed) of
         NONE => TextIO.print "<discard>\n"
       | SOME t => (TextIO.print (Tree.render t); TextIO.print "\n")
   end

fun bindTree gen f = T (fn (size, seed) =>
   case run (gen, size, seed) of
      NONE => NONE
    | SOME t => f t)

fun mapTree f gen = bindTree gen (SOME o f)

fun map f = mapTree (Tree.map f)

fun pure a = T (fn _ => SOME (Tree.pure a))

fun ap (f, m) = T (fn (size, seed) =>
   let
      val (s1, s2) = Seed.split seed
   in
      case run (f, size, s1) of
         NONE => NONE
       | SOME t1 =>
            case run (m, size, s2) of
               NONE => NONE
             | SOME t2 => SOME (Tree.ap (t1, t2))
   end)

fun map2 f (x, y) = T (fn (size, seed) =>
   let
      val (s1, s2) = Seed.split seed
   in
      case run (x, size, s1) of
         NONE => NONE
       | SOME t1 =>
            case run (y, size, s2) of
               NONE => NONE
             | SOME t2 => SOME (Tree.map2 f (t1, t2))
   end)

fun bind m k = T (fn (size, seed) =>
   let
      val (s1, s2) = Seed.split seed
   in
      case run (m, size, s1) of
         NONE => NONE
       | SOME t => Tree.bindOpt t (fn x => run (k x, size, s2))
   end)

fun join m = bind m (fn x => x)

fun mapM _ [] = pure []
  | mapM f (x :: xs) = map2 op :: (f x, mapM f xs)

fun sequence [] = pure []
  | sequence (x :: xs) = map2 op :: (x, sequence xs)

fun fix f =
   let
      val r = ref (T (fn _ => raise Fail "Gen.fix: forced early"))
      val g = T (fn (size, seed) => run (!r, size, seed))
      val res = f g
   in
      r := res
      ; res
   end

val discard = T (fn _ => NONE)

fun ensure p gen =
   bind gen (fn x =>
   if p x
      then pure x
   else discard)

fun or (g1, g2) = T (fn (size, seed) =>
   let
      val (s1, s2) = Seed.split seed
   in
      case run (g1, size, s1) of
         NONE => run (g2, size, s2)
       | SOME t => SOME t
   end)

fun shrink f = mapTree (Tree.expand f)

fun prune gen = mapTree (fn t => Tree.prune (t, 0)) gen

fun sized f = T (fn (size, seed) => run (f size, size, seed))

fun scale f gen = T (fn (size, seed) =>
   let
      val size' = f size
   in
      if size' < 0
         then raise Fail "Gen.scale: negative size"
      else run (gen, size', seed)
   end)

fun resize size gen = scale (fn _ => size) gen

fun small gen =
   let
      fun golden n = Real.round (Real.fromInt n * 0.61803398875 * 0.5)
   in
      scale golden gen
   end

fun recursive f nonrec rec_ =
   sized (fn n =>
      if n <= 1
         then f nonrec
      else f (nonrec @ List.map small rec_))

fun freeze gen = T (fn (size, seed) =>
   case run (gen, size, seed) of
      NONE => NONE
    | SOME t => SOME (Tree.pure (Tree.root t, T (fn _ => SOME t))))

fun mapPartial p gen =
   let
      fun try k =
         if k > 100
            then discard
         else
            bind (freeze (scale (fn n => 2 * k + n) gen))
            (fn (x, gen) =>
               if Option.isSome (p x)
                  then bindTree gen (Tree.mapPartial p)
               else try (k + 1))
   in
      try 0
   end

fun filter p = mapPartial (fn x => if p x then SOME x else NONE)

fun intInfHelper range (size, seed) =
   let
      val (x, y) = Range.bounds (range, size)
   in
      #1 (Seed.nextIntInf (x, y) seed)
   end

fun intInf_ range = T (SOME o Tree.pure o intInfHelper range)

fun halves n =
   Seq.takeWhile (fn k => k <> 0) (Seq.iterate (fn k => IntInf.quot (k, 2)) n)

fun shrinkTowards dest x =
   if dest = x
      then Seq.empty
   else
      let
         val diff = IntInf.div (x, 2) - IntInf.div (dest, 2)
         val rest = Seq.map (fn n => x - n) (halves diff)
      in
         case rest () of
            Seq.Nil => Seq.empty
          | Seq.Cons (x, xs) =>
               if diff = x
                  then Seq.cons (x, xs)
               else Seq.cons (diff, Seq.cons (x, xs))
      end

fun intInf range =
   let
      val origin = Range.origin range

      fun bst (bot, top) =
         let
            val shrinks = shrinkTowards bot top
            val children = Seq.zipWith bst (shrinks, Seq.drop 1 shrinks)
         in
            Tree.Node (top, children)
         end

      fun create root =
         if root = origin
            then Tree.pure root
         else Tree.consChild (origin, bst (origin, root))
   in
      T (fn (size, seed) => SOME (create (intInfHelper range (size, seed))))
   end

val int = map Int.fromLarge o intInf o Range.map Int.toLarge
val int32 = map Int32.fromLarge o intInf o Range.map Int32.toLarge
val int64 = map Int64.fromLarge o intInf o Range.map Int64.toLarge
val word = map Word.fromLargeInt o intInf o Range.map Word.toLargeInt
val word8 = map Word8.fromLargeInt o intInf o Range.map Word8.toLargeInt
val word32 = map Word32.fromLargeInt o intInf o Range.map Word32.toLargeInt
val word64 = map Word64.fromLargeInt o intInf o Range.map Word64.toLargeInt
val int_ = map Int.fromLarge o intInf_ o Range.map Int.toLarge
val int32_ = map Int32.fromLarge o intInf_ o Range.map Int32.toLarge
val int64_ = map Int64.fromLarge o intInf_ o Range.map Int64.toLarge
val word_ = map Word.fromLargeInt o intInf_ o Range.map Word.toLargeInt
val word8_ = map Word8.fromLargeInt o intInf_ o Range.map Word8.toLargeInt
val word32_ = map Word32.fromLargeInt o intInf_ o Range.map Word32.toLargeInt
val word64_ = map Word64.fromLargeInt o intInf_ o Range.map Word64.toLargeInt

fun shrinkTowardsReal dest x =
   if Real.== (dest, x)
      then Seq.empty
   else
      let
         val diff = x - dest
         fun ok y = Real.!= (x, y) andalso Real.isFinite y
      in
         Seq.takeWhile ok (Seq.map (fn n => x - n) (Seq.iterate (fn n => n / 2.0) diff))
      end

fun real_ range = T (fn (size, seed) =>
   let
      val (x, y) = Range.bounds (range, size)
   in
      SOME (Tree.pure (#1 (Seed.nextReal (x, y) seed)))
   end)

fun real range = shrink (shrinkTowardsReal (Range.origin range)) (real_ range)

val bool_ = T (fn (size, seed) =>
   SOME (Tree.pure (Word64.andb (#1 (Seed.nextWord64 seed), 0w1) = 0w0)))

val bool = shrink (fn true => Seq.singleton false | false => Seq.empty) bool_

(**)

fun element [] = raise Fail "Gen.element: empty list"
  | element xs =
   map (fn n => List.nth (xs, n))
      (int (Range.constant {lo = 0, hi = List.length xs - 1}))

fun element_ [] = raise Fail "Gen.element_: empty list"
  | element_ xs =
   map (fn n => List.nth (xs, n))
      (int_ (Range.constant {lo = 0, hi = List.length xs - 1}))

fun choice [] = raise Fail "Gen.choice: empty list"
  | choice xs =
   bind (int (Range.constant {lo = 0, hi = List.length xs - 1}))
      (fn n => List.nth (xs, n))

fun frequency [] = raise Fail "Gen.frequency: empty list"
  | frequency (xs: (int * 'a t) list): 'a t =
   let
      fun pick n [] = raise Fail "Gen.frequency.pick: empty list"
        | pick n ((k, x) :: xs) =
         if n <= k
            then x
         else pick (n - k) xs

      val pick = fn n =>
         (TextIO.print ("pick " ^ Int.toString n ^ "\n");
         pick n)

      val iis =
         let
            fun go q [] = [q]
              | go q ((x, _) :: xs) = q :: go (q + x) xs
         in
            go (#1 (List.hd xs)) (List.tl xs)
         end

      val total = List.foldl (fn ((k, _), n) => n + k) 0 xs

      fun shrinker n =
         let
            val iis' = List.filter (fn k => k < n) iis
         in
            Seq.fromList iis'
         end
   in
      bind
         (shrink shrinker
            (int_ (Range.constant {lo = 1, hi = total})))
         (fn n => pick n xs)
   end

(**)

fun atLeast n xs =
   if n <= 0
      then true
   else
      case xs of
         [] => false
       | _ :: ys => atLeast (n - 1) ys

fun replicate' n gen =
   if n <= 0
      then pure []
   else map2 op :: (gen, replicate' (n - 1) gen)

fun list range (gen: 'a t) =
   sized (fn size =>
      ensure (atLeast (Range.lowerBound (range, size)))
         (T (fn (size, seed) =>
            let
               fun genTrees (n, seed) =
                  if n <= 0
                     then []
                  else
                     let
                        val (s, seed) = Seed.split seed
                     in
                        case run (gen, size, s) of
                           NONE => genTrees (n - 1, seed)
                         | SOME t => t :: genTrees (n - 1, seed)
                     end

               val (s, seed) = Seed.split seed
               val n = IntInf.toInt (intInfHelper (Range.map Int.toLarge range) (size, s))
            in
               SOME (Tree.interleave (genTrees (n, seed)))
            end)))

(**)

structure Subterms =
   struct
      type 'a gen = 'a t
      datatype 'a t = One of 'a | All of 'a list

      fun shrinker (One _) = Seq.empty
        | shrinker (All xs) = Seq.map One (Seq.fromList xs)

      fun gen gs =
         bind (shrink shrinker (map All (mapM (map #2 o freeze) gs)))
            (fn One g => map One g
              | All gs => map All (sequence gs))
   end

fun subtermM g f =
   bind (Subterms.gen [g])
      (fn Subterms.One x => pure x
        | Subterms.All [x] => f x
        | _ => raise Fail "Gen.subtermM: internal error")

fun subterm g f = subtermM g (pure o f)

fun subtermM2 g1 g2 f =
   bind (Subterms.gen [g1, g2])
      (fn Subterms.One x => pure x
        | Subterms.All [x, y] => f (x, y)
        | _ => raise Fail "Gen.subtermM2: internal error")

fun subterm2 g1 g2 f = subtermM2 g1 g2 (pure o f)

fun subtermM3 g1 g2 g3 f =
   bind (Subterms.gen [g1, g2, g3])
      (fn Subterms.One x => pure x
        | Subterms.All [x, y, z] => f (x, y, z)
        | _ => raise Fail "Gen.subtermM3: internal error")

fun subterm3 g1 g2 g3 f = subtermM3 g1 g2 g3 (pure o f)

(**)

val binDigit = map (fn true => #"1" | false => #"0") bool
val octDigit = map (fn n => Char.chr (Char.ord #"0" + n)) (int (Range.constant {lo = 0, hi = 7}))
val digit = map (fn n => Char.chr (Char.ord #"0" + n)) (int (Range.constant {lo = 0, hi = 9}))
val hexDigit = map (fn n => Char.chr (if n < 10 then Char.ord #"0" + n else Char.ord #"A" - 10 + n)) (int (Range.constant {lo = 0, hi = 16}))
val lowerChar = map (fn n => Char.chr (Char.ord #"a" + n)) (int (Range.constant {lo = 0, hi = 25}))
val upperChar = map (fn n => Char.chr (Char.ord #"A" + n)) (int (Range.constant {lo = 0, hi = 25}))

local
   val alphaChars = "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   val alphaNumChars = "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
in
   val alphaChar = map (fn n => String.sub (alphaChars, n)) (int (Range.constant {lo = 0, hi = String.size alphaChars - 1}))
   val alphaNumChar = map (fn n => String.sub (alphaNumChars, n)) (int (Range.constant {lo = 0, hi = String.size alphaNumChars - 1}))
end

val asciiChar = map Char.chr (int (Range.constant {lo = 0, hi = 127}))
val latin1Char = map Char.chr (int (Range.constant {lo = 0, hi = 255}))
val unicodeChar = frequency
   [(55296, word (Range.constant {lo = 0w0, hi = 0w55295})),
    (8190, word (Range.constant {lo = 0w57344, hi = 0w65533})),
    (1048576, word (Range.constant {lo = 0w65536, hi = 0w1114111}))]

end

(* vim: set tw=0 ts=3 sw=3: *)

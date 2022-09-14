
structure Observer: OBSERVER =
struct

type 'a t = 'a Types.observer
type 'a generator = 'a Types.generator

val observe = Types.observe
val create = Types.Obs

fun contramap f obs =
   create (fn (x, n, seed) => observe (obs, f x, n, seed))

fun fix f =
   let
      val r = ref (create (fn _ => raise Fail "Observer.fix: forced too early"))
      val obs = create (fn (x, size, seed) => observe (!r, x, size, seed))
      val res = f obs
   in
      r := res
      ; res
   end

fun hashCombine (seed, hash) =
   let
      open Word64
      infix xorb << >>
   in
      seed xorb (hash + 0wx9e3779b9 + (seed << 0w6) + (seed >> 0w2))
   end

val unit = create (fn ((), _, seed) => seed)
val bool = create (fn (b, _, seed) => hashCombine (seed, if b then 0w1 else 0w2))
val char = create (fn (c, _, seed) => hashCombine (seed, Word64.fromInt (Char.ord c)))
val int = create (fn (n, _, seed) => hashCombine (seed, Word64.fromInt n))
val int32 = create (fn (n, _, seed) => hashCombine (seed, Word64.fromInt (Int32.toInt n)))
val int64 = create (fn (n, _, seed) => hashCombine (seed, Word64.fromLargeInt (Int64.toLarge n)))
val word = create (fn (w, _, seed) => hashCombine (seed, Word.toLarge w))
val word8 = create (fn (w, _, seed) => hashCombine (seed, Word8.toLarge w))
val word32 = create (fn (w, _, seed) => hashCombine (seed, Word32.toLarge w))
val word64 = create (fn (w, _, seed) => hashCombine (seed, w))

fun pair (obs1, obs2) =
   create (fn ((a, b), size, seed) =>
      observe (obs2, b, size, observe (obs1, a, size, seed)))

fun either (obs1, obs2) =
   create (fn (e, size, seed) =>
      case e of
         Either.INL a => observe (obs1, a, size, hashCombine (seed, 0w1))
       | Either.INR b => observe (obs2, b, size, hashCombine (seed, 0w2)))

fun option obs =
   create (fn (opt, size, seed) =>
      case opt of
         NONE => hashCombine (seed, 0w1)
       | SOME a => observe (obs, a, size, hashCombine (seed, 0w2)))

fun list obs =
   create (fn (xs, size, seed) =>
      let
         val len = List.length xs
         val rng = SplitMix.fromSeed seed
         val sizes = Generator.generate (Generator.sizes (len, len), size, rng)
      in
         ListPair.foldlEq
            (fn (x, size, seed) =>
               observe (obs, x, size, hashCombine (seed, 0w1)))
            (hashCombine (seed, 0w0))
            (xs, sizes)
      end)

fun vector obs = contramap Vector.toList (list obs)

fun func (domain, range) =
   create (fn (f, size, seed) =>
      let
         val rng = SplitMix.fromSeed seed
         val inputRange = (0, 4294967296)
         val sizes = Generator.generate (Generator.sizes inputRange, size * 2, rng)
      in
         List.foldl
            (fn (size, seed) =>
               let
                  val x = Generator.generate (domain, size, rng)
               in
                  observe (range, f x, size, seed)
               end)
            seed
            sizes
      end)

end

(* vim: set tw=0 ts=3 sw=3: *)

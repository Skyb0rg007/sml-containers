
functor MonoMapAdapterFn(S: MONO_MAP_ADAPTER_STRUCTS)
   :> MONO_MAP where type key = S.key and type value = S.value =
struct

open S

type map = Map.map

val empty = Map.empty
fun singleton (k, x) = Map.singleton (Key.adapt k, (k, x))

fun insert (m, k, x) = Map.insert (m, Key.adapt k, (k, x))

fun insertWith f (m, k, x) =
   Map.insertWith
      (fn ((k, x), (_, y)) => (k, f (x, y)))
      (m, Key.adapt k, (k, x))

fun insertWithi f (m, k, x) =
   Map.insertWith
      (fn ((k, x), (_, y)) => (k, f (k, x, y)))
      (m, Key.adapt k, (k, x))

fun insert' ((k, x), m) = insert (m, k, x)

fun insertLookupWithi f (m, k, x) =
   let
      fun f' (_, (k, x), (_, y)) = (k, f (k, x, y))
      val (old, m') = Map.insertLookupWithi f' (m, Key.adapt k, (k, x))
   in
      (Option.map #2 old, m')
   end

fun fromList xs = List.foldl insert' empty xs
fun fromListWith f = List.foldl (fn ((k, x), m) => insertWith f (m, k, x)) empty
fun fromListWithi f = List.foldl (fn ((k, x), m) => insertWithi f (m, k, x)) empty

fun delete (m, k) = Map.delete (m, Key.adapt k)

fun remove (m, k) =
   case Map.remove (m, Key.adapt k) of
      NONE => NONE
    | SOME (m', (_, x)) => SOME (m', x)

fun adjust f (m, k) =
   Map.adjust (fn (k, x) => (k, f x)) (m, Key.adapt k)

fun update f (m, k) =
   Map.update
      (fn (k, x) => case f x of NONE => NONE | SOME y => SOME (k, y))
      (m, Key.adapt k)

fun updateLookupWithi f (m, k) =
   let
      fun f' (_, (k, x)) =
         case f (k, x) of
            NONE => NONE
          | SOME y => SOME (k, y)
      val (old, m') = Map.updateLookupWithi f' (m, Key.adapt k)
   in
      (Option.map #2 old, m')
   end

fun alter f (m, k) =
   let
      fun f' NONE =
         (case f NONE of NONE => NONE | SOME y => SOME (k, y))
        | f' (SOME (k, x)) =
         (case f (SOME x) of NONE => NONE | SOME y => SOME (k, y))
   in
      Map.alter f' (m, Key.adapt k)
   end

fun find (m, k) = Option.map #2 (Map.find (m, Key.adapt k))

fun inDomain (m, k) = Map.inDomain (m, Key.adapt k)

val isEmpty = Map.isEmpty
val size = Map.size

fun unionWith f =
   Map.unionWith (fn ((k, x), (_, y)) => (k, f (x, y)))

fun unionWithi f =
   Map.unionWith (fn ((k, x), (_, y)) => (k, f (k, x, y)))

val difference = Map.difference

fun differenceWith f =
   Map.differenceWith (fn ((k, x), (_, y)) =>
      case f (x, y) of NONE => NONE | SOME z => SOME (k, z))

fun differenceWithi f =
   Map.differenceWith (fn ((k, x), (_, y)) =>
      case f (k, x, y) of NONE => NONE | SOME z => SOME (k, z))

val intersection = Map.intersection

fun intersectionWith f =
   Map.intersectionWith (fn ((k, x), (_, y)) =>
      case f (x, y) of NONE => NONE | SOME z => SOME (k, z))

fun intersectionWithi f =
   Map.intersectionWith (fn ((k, x), (_, y)) =>
      case f (k, x, y) of NONE => NONE | SOME z => SOME (k, z))

val disjoint = Map.disjoint

fun map f = Map.map (fn (k, x) => (k, f x))
fun mapi f = Map.map (fn (k, x) => (k, f (k, x)))

fun mapAccumL f =
   Map.mapAccumL (fn ((k, x), acc) => case f (x, acc) of (acc', y) => (acc', (k, y)))
fun mapAccumLi f =
   Map.mapAccumL (fn ((k, x), acc) => case f (k, x, acc) of (acc', y) => (acc', (k, y)))
fun mapAccumR f =
   Map.mapAccumR (fn ((k, x), acc) => case f (x, acc) of (acc', y) => (acc', (k, y)))
fun mapAccumRi f =
   Map.mapAccumR (fn ((k, x), acc) => case f (k, x, acc) of (acc', y) => (acc', (k, y)))

fun foldl f = Map.foldl (fn ((_, x), acc) => f (x, acc))
fun foldli f = Map.foldl (fn ((k, x), acc) => f (k, x, acc))
fun foldr f = Map.foldr (fn ((_, x), acc) => f (x, acc))
fun foldri f = Map.foldr (fn ((k, x), acc) => f (k, x, acc))
fun app f = Map.app (fn (_, x) => f x)
val appi = Map.app
fun exists f = Map.exists (fn (_, x) => f x)
val existsi = Map.exists
fun all f = Map.all (fn (_, x) => f x)
val alli = Map.all

fun keys m = foldli (fn (k, _, ks) => k :: ks) [] m
fun elems m = foldl op :: [] m
fun toList m = foldli (fn (k, x, kxs) => (k, x) :: kxs) [] m

fun filter f = Map.filter (fn (_, x) => f x)

val filteri = Map.filter

fun mapPartial f =
   Map.mapPartial (fn (k, x) =>
      case f x of
         NONE => NONE
       | SOME y => SOME (k, y))

fun mapPartiali f =
   Map.mapPartial (fn (k, x) =>
      case f (k, x) of
         NONE => NONE
       | SOME y => SOME (k, y))

fun mapEither f =
   Map.mapEither (fn (k, x) =>
      case f x of
         Either.INL y => Either.INL (k, y)
       | Either.INR z => Either.INR (k, z))

fun mapEitheri f =
   Map.mapEither (fn (k, x) =>
      case f (k, x) of
         Either.INL y => Either.INL (k, y)
       | Either.INR z => Either.INR (k, z))

fun partition f = Map.partition (fn (_, x) => f x)
val partitioni = Map.partition

fun isSubmapBy cmp = Map.isSubmapBy (fn ((_, x), (_, y)) => cmp (x, y))

fun isProperSubmapBy cmp = Map.isProperSubmapBy (fn ((_, x), (_, y)) => cmp (x, y))

fun liftEquals eq = Map.liftEquals (fn ((_, x), (_, y)) => eq (x, y))

fun collate cmp = Map.collate (fn ((_, x), (_, y)) => cmp (x, y))

end

(* vim: set tw=0 ts=3 sw=3: *)

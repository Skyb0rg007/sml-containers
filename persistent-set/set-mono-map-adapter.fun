
functor SetMonoMapAdapterFn(Map: MONO_MAP where type value = unit): SET =
struct

type item = Map.key
type set = Map.map

val empty = Map.empty
fun singleton x = Map.singleton (x, ())
fun insert (s, x) = Map.insert (s, x, ())
val fromList = List.foldl (fn (x, s) => insert (s, x)) empty
val delete = Map.delete
val member = Map.inDomain
val size = Map.size
val isEmpty = Map.isEmpty
val union = Map.unionWith (fn ((), ()) => ())
val difference = Map.difference
val intersection = Map.intersection
val disjoint = Map.disjoint
fun foldl f = Map.foldli (fn (x, _, acc) => f (x, acc))
fun foldr f = Map.foldri (fn (x, _, acc) => f (x, acc))
fun app f = Map.appi (fn (x, _) => f x)
fun all f = Map.alli (fn (x, _) => f x)
fun exists f = Map.existsi (fn (x, _) => f x)
val toList = Map.keys
fun filter f = Map.filteri (fn (x, _) => f x)
fun partition f = Map.partitioni (fn (x, _) => f x)
val isSubmap = Map.isSubmapBy (fn ((), ()) => true)
val isProperSubmap = Map.isProperSubmapBy (fn ((), ()) => true)
val equals = Map.liftEquals (fn ((), ()) => true)
val compare = Map.collate (fn ((), ()) => EQUAL)

end

(* vim: set tw=0 ts=3 sw=3: *)


functor HAMTFn(Key: HASHABLE_TYPE): MAP =
struct

type key = Key.t

datatype 'a map =
   Empty
 | Bitmap of word * 'a map vector
 | Full of 'a map vector
 | Leaf of word * key * 'a
 | Collision of word * (key * 'a) vector

(* Helpers *)

fun vectorInsert (v, i, x) =
   let
      fun go idx =
         case Int.compare (idx, i) of
            LESS => Vector.sub (v, idx)
          | EQUAL => x
          | GREATER => Vector.sub (v, idx - 1)
   in
      Vector.tabulate (Vector.length v + 1, go)
   end

fun vectorDelete (v, i) =
   let
      fun go idx =
         if idx < i
            then Vector.sub (v, idx)
         else Vector.sub (v, idx + 1)
   in
      Vector.tabulate (Vector.length v - 1, go)
   end

fun vectorSnoc (v, x) =
   let
      val len = Vector.length v

      fun go idx =
         if idx = len
            then x
         else Vector.sub (v, idx)
   in
      Vector.tabulate (Vector.length v + 1, go)
   end

fun vectorEquals eq (v1, v2) =
   let
      val len1 = Vector.length v1
      val len2 = Vector.length v2

      fun go i =
         if i = len1
            then true
         else eq (Vector.sub (v1, i), Vector.sub (v2, i)) andalso go (i + 1)
   in
      len1 = len2 andalso go 0
   end

val bitsPerSubkey = 0w5
val subkeyMask = 0wx1f
val maxChildren = 0w32
val fullNodeMask = 0wxffffffff

(* Bitwise *)
fun index (w, s) = Word.andb (Word.>> (w, s), subkeyMask)
fun index' (w, s) = Word.toInt (index (w, s))
fun mask (w, s) = Word.<< (0w1, index (w, s))
fun sparseIndex (b, m) = WordEx.popCount (Word.andb (b, m - 0w1))

(* Create a hashmap from two hash-key-value triples at the given shift level *)
fun two (s, (h, k, x), (h', k', x')) =
   let
      val leaf = Leaf (h, k, x)
      val leaf' = Leaf (h', k', x')

      fun go s =
         let
            val b = mask (h, s)
            val b' = mask (h', s)
         in
            if b = b'
               then Bitmap (b, #[go (s + bitsPerSubkey)])
            else if index (h, s) < index (h', s)
               then Bitmap (Word.orb (b, b'), #[leaf, leaf'])
            else Bitmap (Word.orb (b, b'), #[leaf', leaf])
         end
   in
      go s
   end

fun bitmap (b, arr) =
   if b = fullNodeMask
      then Full arr
   else Bitmap (b, arr)

fun isLeafOrCollision (Leaf _) = true
  | isLeafOrCollision (Collision _) = true
  | isLeafOrCollision _ = false

(** Exports **)

val empty = Empty

fun singleton (k, x) = Leaf (Key.hash k, k, x)

fun insertLookupCollisionWithi f (kxs, k, x) =
   let
      val len = Vector.length kxs
      fun go i =
         if i >= len
            then (NONE, vectorSnoc (kxs, (k, x)))
         else if Key.equals (k, #1 (Vector.sub (kxs, i)))
            then (SOME (#2 (Vector.sub (kxs, i))), Vector.update (kxs, i, (k, x)))
         else go (i + 1)
   in
      go 0
   end

fun insertCollisionWithi f (kxs, k, x) =
   let
      val len = Vector.length kxs
      fun go i =
         if i >= len
            then vectorSnoc (kxs, (k, x))
         else if Key.equals (k, #1 (Vector.sub (kxs, i)))
            then Vector.update (kxs, i, (k, x))
         else go (i + 1)
   in
      go 0
   end

fun insertLookupWithi f (t, k, x) =
   let
      val h = Key.hash k

      fun go (_, Empty) = (NONE, Leaf (h, k, x))
        | go (s, Leaf (h', k', x')) =
         if h = h'
            then
               if Key.equals (k, k')
                  then (SOME x', Leaf (h, k, f (k, x', x)))
               else (NONE, Collision (h, #[(k', x'), (k, x)]))
         else (NONE, two (s, (h, k, x), (h', k', x')))
        | go (s, coll as Collision (h', kxs)) =
         if h = h'
            then 
               let
                  val (prev, kxs') = insertLookupCollisionWithi f (kxs, k, x)
               in
                  (prev, Collision (h, kxs'))
               end
         else go (s, Bitmap (mask (h', s), #[coll]))
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
            val i = sparseIndex (b, m)
         in
            if Word.andb (b, m) <> 0w0
               then (NONE, bitmap (Word.orb (b, m), vectorInsert (arr, i, Leaf (h, k, x))))
            else
               let
                  val st = Vector.sub (arr, i)
                  val (prev, st') = go (s + bitsPerSubkey, st)
               in
                  (prev, Bitmap (b, Vector.update (arr, i, st')))
               end
         end
        | go (s, Full arr) =
         let
            val i = index' (h, s)
            val st = Vector.sub (arr, i)
            val (prev, st') = go (s + bitsPerSubkey, st)
         in
            (prev, Full (Vector.update (arr, i, st')))
         end
   in
      go (0w0, t)
   end

fun insertWithi f (t, k, x) =
   let
      val h = Key.hash k

      fun go (_, Empty) = Leaf (h, k, x)
        | go (s, Leaf (h', k', x')) =
         if h = h'
            then
               if Key.equals (k, k')
                  then Leaf (h, k, f (k, x', x))
               else Collision (h, #[(k', x'), (k, x)])
         else two (s, (h, k, x), (h', k', x'))
        | go (s, coll as Collision (h', kxs)) =
         if h = h'
            then Collision (h, insertCollisionWithi f (kxs, k, x))
         else go (s, Bitmap (mask (h', s), #[coll]))
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
            val i = sparseIndex (b, m)
         in
            if Word.andb (b, m) <> 0w0
               then bitmap (Word.orb (b, m), vectorInsert (arr, i, Leaf (h, k, x)))
            else
               let
                  val st = Vector.sub (arr, i)
                  val st' = go (s + bitsPerSubkey, st)
               in
                  Bitmap (b, Vector.update (arr, i, st'))
               end
         end
        | go (s, Full arr) =
         let
            val i = index' (h, s)
            val st = Vector.sub (arr, i)
            val st' = go (s + bitsPerSubkey, st)
         in
            Full (Vector.update (arr, i, st'))
         end
   in
      go (0w0, t)
   end

fun insertWith f = insertWithi (fn (_, x', x) => f (x', x))
fun insert (t, k, x) = insertWithi (fn (_, _, x) => x) (t, k, x)
fun insert' ((k, x), t) = insert (t, k, x)

fun fromList xs = List.foldl insert' empty xs
fun fromListWith f xs = List.foldl (fn ((k, x), m) => insertWith f (m, k, x)) empty xs
fun fromListWithi f xs = List.foldl (fn ((k, x), m) => insertWithi f (m, k, x)) empty xs

fun remove (t, k) =
   let
      val h = Key.hash k

      fun go (_, Empty) = NONE
        | go (_, Leaf (h', k', x)) =
         if h = h' andalso Key.equals (k, k')
            then SOME (Empty, x)
         else NONE
        | go (s, Collision (h', kxs)) =
         if h = h'
            then
               case Vector.findi (fn (_, (k', _)) => Key.equals (k, k')) kxs of
                  NONE => NONE
                | SOME (i, (_, x)) =>
                     if Vector.length kxs <> 2
                        then SOME (Collision (h, vectorDelete (kxs, i)), x)
                     else 
                        let
                           val j = if i = 0 then 1 else 0
                           val (k', x') = Vector.sub (kxs, j)
                        in
                           SOME (Leaf (h, k', x'), x)
                        end
         else NONE
        | go (s, Full arr) =
         let
            val idx = index (h, s)
            val i = Word.toInt idx
            val st = Vector.sub (arr, i)
         in
            case go (s + bitsPerSubkey, st) of
               NONE => NONE
             | SOME (Empty, x) =>
                  let
                     val b = Word.andb (fullNodeMask, Word.notb (Word.<< (0w1, idx)))
                  in
                     SOME (Bitmap (b, vectorDelete (arr, i)), x)
                  end
             | SOME (st', x) => SOME (Full (Vector.update (arr, i, st')), x)
         end
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
         in
            if Word.andb (b, m) = 0w0
               then NONE
            else
               let
                  val i = sparseIndex (b, m)
                  val st = Vector.sub (arr, i)
                  fun delIndex () =
                     Bitmap (Word.andb (b, Word.notb m), vectorDelete (arr, i))
               in
                  case (go (s + bitsPerSubkey, st), Vector.length arr) of
                     (NONE, _) => NONE
                     (* Preserve invariant: Nodes cannot have just 1 child if its not a node *)
                   | (SOME (Empty,              x), 1) => SOME (Empty, x)
                   | (SOME (st' as Leaf _,      x), 1) => SOME (st', x)
                   | (SOME (st' as Collision _, x), 1) => SOME (st', x)
                     (* The node had 2 children and it's being removed: check invariants *)
                   | (SOME (Empty, x), 2) =>
                        if i = 0 andalso isLeafOrCollision (Vector.sub (arr, 1))
                           then SOME (Vector.sub (arr, 1), x)
                        else if i = 1 andalso isLeafOrCollision (Vector.sub (arr, 0))
                           then SOME (Vector.sub (arr, 0), x)
                        else SOME (delIndex (), x)
                     (* Otherwise, just remove/update the index *)
                   | (SOME (Empty, x), _) => SOME (delIndex (), x)
                   | (SOME (st', x), _) => SOME (Bitmap (b, Vector.update (arr, i, st')), x)
               end
         end
   in
      go (0w0, t)
   end

fun delete (t, k) =
   case remove (t, k) of
      NONE => t
    | SOME (t', _) => t'

fun adjust f (t, k) =
   let
      val h = Key.hash k

      fun go (_, Empty) = Empty
        | go (_, t as Leaf (h', k', x)) =
         if h = h' andalso Key.equals (k, k')
            then Leaf (h, k, f x)
         else t
        | go (_, t as Collision (h', kxs)) =
         if h = h'
            then
               case Vector.findi (fn (_, (k', _)) => Key.equals (k, k')) kxs of
                  NONE => t
                | SOME (i, (_, x)) =>
                     Collision (h, Vector.update (kxs, i, (k, f x)))
         else t
        | go (s, t as Bitmap (b, arr)) =
         let
            val m = mask (h, s)
            val i = sparseIndex (b, m)
         in
            if Word.andb (b, m) = 0w0
               then t
            else
               let
                  val st = Vector.sub (arr, i)
                  val st' = go (s + bitsPerSubkey, st)
               in
                  Bitmap (b, Vector.update (arr, i, st'))
               end
         end
        | go (s, Full arr) =
         let
            val i = index' (h, s)
            val st = Vector.sub (arr, i)
            val st' = go (s + bitsPerSubkey, st)
         in
            Full (Vector.update (arr, i, st'))
         end
   in
      go (0w0, t)
   end

fun update _ = raise Fail "NYI"
fun updateLookupWithi _ = raise Fail "NYI"
fun alter _ = raise Fail "NYI"

fun find' (s, t, h, k) =
   let
      fun go (_, Empty) = NONE
        | go (_, Leaf (h', k', x)) =
         if h = h' andalso Key.equals (k, k')
            then SOME x
         else NONE
        | go (s, Bitmap (b, arr)) =
         let
            val m = mask (h, s)
         in
            if Word.andb (b, m) = 0w0
               then NONE
            else go (s + bitsPerSubkey, Vector.sub (arr, sparseIndex (b, m)))
         end
        | go (s, Full arr) =
         go (s + bitsPerSubkey, Vector.sub (arr, index' (h, s)))
        | go (s, Collision (h', kxs)) =
         if h = h'
            then
               case Vector.find (fn (k', _) => Key.equals (k, k')) kxs of
                  NONE => NONE
                | SOME (_, x) => SOME x
         else NONE
   in
      go (s, t)
   end

fun find (t, k) = find' (0w0, t, Key.hash k, k)

fun inDomain (t, k) = Option.isSome (find (t, k))

fun isEmpty Empty = true
  | isEmpty _ = false

fun size t =
   let
      fun go (Empty, n) = n
        | go (Leaf _, n) = n + 1
        | go (Bitmap (_, arr), n) = Vector.foldl go n arr
        | go (Full arr, n) = Vector.foldl go n arr
        | go (Collision (_, arr), n) = n + Vector.length arr
   in
      go (t, 0)
   end

fun concatCollisionWithi f (arr1, arr2) =
   let
      fun findIn1 k = Vector.findi (fn (_, (k', _)) => Key.equals (k, k')) arr1
      (* The positions of elements of arr2 in arr1 *)
      val indices = Vector.map (fn (k, _) => Option.map #1 (findIn1 k)) arr2
      (* Number of elements in arr2 which aren't in arr1 *)
      val nOnly2 = Vector.foldl (fn (NONE, n) => n + 1 | (SOME _, n) => n) 0 indices
      val n1 = Vector.length arr1
      val n2 = Vector.length arr2
      val {done, update, ...} = VectorEx.create (n1 + nOnly2)
      (* Copy all of arr1 into result *)
      val () = Vector.appi update arr1
      (* iEnd is the next unwritten element of result
      * i2 is the next unread element of arr2 *)
      fun go (iEnd, i2) =
         if i2 >= n2
            then ()
         else
            case Vector.sub (indices, i2) of
               (* Element is in arr2 at i2, but not in arr1 *)
               NONE => (update (iEnd, Vector.sub (arr2, i2)); go (iEnd + 1, i2 + 1))
               (* Element is in arr2 at i2, and in arr1 at index i1 *)
             | SOME i1 =>
                  let
                     val (k, x1) = Vector.sub (arr1, i1)
                     val (_, x2) = Vector.sub (arr2, i2)
                     val () = update (i1, (k, f (k, x1, x2)))
                  in
                     go (iEnd, i2 + 1)
                  end
      val () = go (n1, 0)
   in
      done ()
   end

fun unionArrayBy f (b1, b2, arr1, arr2) =
   let
      val bs = Word.orb (b1, b2)
      val len = WordEx.popCount bs
      val {done, update, ...} = VectorEx.create len

      fun go (_, _, _, 0w0) = ()
        | go (i, j1, j2, b) =
         let
            val m = Word.<< (0w1, Word.fromInt (WordEx.trailingZeros b))
            fun testBit x = Word.andb (x, m) <> 0w0
            val b' = Word.andb (b, Word.notb m)
         in
            if testBit (Word.andb (b1, b2))
               then (update (i, f (Vector.sub (arr1, j1), Vector.sub (arr2, j2)))
                     ; go (i + 1, j1 + 1, j2 + 1, b'))
            else if testBit b1
               then (update (i, Vector.sub (arr1, j1))
                     ; go (i + 1, j1 + 1, j2, b'))
            else (update (i, (Vector.sub (arr2, j2)))
                  ; go (i + 1, j1, j2 + 1, b'))
         end

      val () = go (0, 0, 0, bs)
   in
      done ()
   end

fun unionWithi f =
    let
      fun leafHash (Leaf (h, _, _)) = h
        | leafHash (Collision (h, _)) = h
        | leafHash _ = raise Fail "unionWithi: leafHash given non-leaf!"

      fun goDifferentHash (s, h, h', t, t') =
          let
            val m = mask (h, s)
            val m' = mask (h', s)
          in
            case Word.compare (m, m') of
               EQUAL => Bitmap (m, #[goDifferentHash (s + bitsPerSubkey, h, h', t, t')])
             | LESS => Bitmap (Word.orb (m, m'), #[t, t'])
             | GREATER => Bitmap (Word.orb (m, m'), #[t', t])
          end

        (* empty vs anything *)
      fun go _ (t, Empty) = t
        | go _ (Empty, t') = t'
        (* leaf vs leaf *)
        | go s (t as Leaf (h, k, x), t' as Leaf (h', k', x')) =
            if h = h'
              then if Key.equals (k, k')
                     then Leaf (h, k, f (k, x, x'))
                   else Collision (h, #[(k, x), (k', x')])
            else goDifferentHash (s, h, h', t, t')
        | go s (t as Leaf (h, k, x), t' as Collision (h', kxs')) =
            if h = h'
              (* Need to swap arguments since the collision is the right side *)
              then Collision (h, insertCollisionWithi (fn (k, x, y) => f (k, y, x)) (kxs', k, x))
            else goDifferentHash (s, h, h', t, t')
        | go s (t as Collision (h, kxs), t' as Leaf (h', k', x')) =
            if h = h'
              then Collision (h, insertCollisionWithi f (kxs, k', x'))
            else goDifferentHash (s, h, h', t, t')
        | go s (t as Collision (h, kxs), t' as Collision (h', kxs')) =
            if h = h'
              then Collision (h, concatCollisionWithi f (kxs, kxs'))
            else goDifferentHash (s, h, h', t, t')
        (* branch vs branch *)
        | go s (Bitmap (b, arr), Bitmap (b', arr')) =
            Bitmap (Word.orb (b, b'), unionArrayBy (go (s + bitsPerSubkey)) (b, b', arr, arr'))
        | go s (Bitmap (b, arr), Full arr') =
            Full (unionArrayBy (go (s + bitsPerSubkey)) (b, fullNodeMask, arr, arr'))
        | go s (Full arr, Bitmap (b', arr')) =
            Full (unionArrayBy (go (s + bitsPerSubkey)) (fullNodeMask, b', arr, arr'))
        | go s (Full arr, Full arr') =
            Full (unionArrayBy (go (s + bitsPerSubkey)) (fullNodeMask, fullNodeMask, arr, arr'))
        (* leaf vs branch *)
        | go s (Bitmap (b, arr), t') =
          let
            val h' = leafHash t'
            val m' = mask (h', s)
            val i = sparseIndex (b, m')
          in
            if Word.andb (b, m') = 0w0
              then bitmap (Word.orb (b, m'), vectorInsert (arr, i, t'))
            else Bitmap (b, Vector.update (arr, i, go (s + bitsPerSubkey) (Vector.sub (arr, i), t')))
          end
        | go s (t, Bitmap (b', arr')) =
          let
            val h = leafHash t
            val m = mask (h, s)
            val i = sparseIndex (b', m)
          in
            if Word.andb (b', m) = 0w0
              then bitmap (Word.orb (b', m), vectorInsert (arr', i, t))
            else Bitmap (b', Vector.update (arr', i, go (s + bitsPerSubkey) (t, Vector.sub (arr', i))))
          end
        | go s (Full arr, t') =
          let
            val h' = leafHash t'
            val i = index' (h', s)
          in
            Full (Vector.update (arr, i, go (s + bitsPerSubkey) (Vector.sub (arr, i), t')))
          end
        | go s (t, Full arr') =
          let
            val h = leafHash t
            val i = index' (h, s)
          in
            Full (Vector.update (arr', i, go (s + bitsPerSubkey) (t, Vector.sub (arr', i))))
          end
    in
      go 0w0
    end

fun unionWith f = unionWithi (fn (_, x, x') => f (x, x'))

fun foldli f z t =
   let
      fun f' ((k, x), acc) = f (k, x, acc)

      fun go (Empty, acc) = acc
        | go (Leaf (_, k, x), acc) = f (k, x, acc)
        | go (Bitmap (_, arr), acc) = Vector.foldl go acc arr
        | go (Full arr, acc) = Vector.foldl go acc arr
        | go (Collision (_, kxs), acc) = Vector.foldl f' acc kxs
   in
      go (t, z)
   end

fun foldri f z t =
   let
      fun f' ((k, x), acc) = f (k, x, acc)

      fun go (Empty, acc) = acc
        | go (Leaf (_, k, x), acc) = f (k, x, acc)
        | go (Bitmap (_, arr), acc) = Vector.foldr go acc arr
        | go (Full arr, acc) = Vector.foldr go acc arr
        | go (Collision (_, kxs), acc) = Vector.foldr f' acc kxs
   in
      go (t, z)
   end

fun appi f =
   let
      fun go Empty = ()
        | go (Leaf (_, k, x)) = f (k, x)
        | go (Bitmap (_, arr)) = Vector.app go arr
        | go (Full arr) = Vector.app go arr
        | go (Collision (_, kxs)) = Vector.app f kxs
   in
      go
   end

fun existsi f =
  let
     fun go Empty = false
       | go (Leaf (_, k, x)) = f (k, x)
       | go (BitmapIndexed (_, arr)) = Vector.exists go arr
       | go (Full arr) = Vector.exists go arr
       | go (Collision (_, kxs)) = Vector.exists f kxs
  in
     go
  end

fun alli f =
  let
     fun go Empty = true
       | go (Leaf (_, k, x)) = f (k, x)
       | go (BitmapIndexed (_, arr)) = Vector.all go arr
       | go (Full arr) = Vector.all go arr
       | go (Collision (_, kxs)) = Vector.all f kxs
  in
     go
  end

fun foldl f = foldli (fn (_, x, a) => f (x, a))
fun foldr f = foldri (fn (_, x, a) => f (x, a))
fun app f = appi (fn (_, x) => f x)
fun exists f = existsi (fn (_, x) => f x)
fun all f = alli (fn (_, x) => f x)
fun listItemsi t = foldri (fn (k, x, z) => (k, x) :: z) [] t
fun listItems t = foldr op :: [] t
fun listKeys t = foldri (fn (k, _, z) => k :: z) [] t

fun differenceWithi f (t, t') =
   let
      fun go (k, x, m) =
         case find (t', k) of
            NONE => insert (m, k, x)
          | SOME x' =>
               case f (k, x, x') of
                  NONE => m
                | SOME y => insert (m, k, y)
   in
      foldli go empty t
   end

fun differenceWith f = differenceWithi (fn (_, x, x') => f (x, x'))

fun difference (t, t') =
   let
      fun go (k, x, t) =
         if inDomain (t', k)
            then t
         else insert (t, k, x)
   in
      foldli go empty t
   end

fun intersectionWithi f = raise Fail "NYI"
   (* let *)
   (*    fun go _ (_, Empty) = Empty *)
   (*      | go _ (Empty, _) = Empty *)
   (*      | go s (Leaf (h, k, x1), t2) = *)
   (*       (case find' (s, t2, h, k) of *)
   (*           NONE => Empty *)
   (*         | SOME x2 => *)
   (*              case f (k, x1, x2) of *)
   (*                 NONE => Empty *)
   (*               | SOME y => Leaf (h, k, y)) *)
   (*      | go s (t1, Leaf (h, k, x2)) = *)
   (*       (case find' (s, t1, h, k) of *)
   (*           NONE => Empty *)
   (*         | SOME x1 => *)
   (*              case f (k, x1, x2) of *)
   (*                 NONE => Empty *)
   (*               | SOME y => Leaf (h, k, y)) *)
   (*      | go _ (Collision kxs1, Collision kxs2) = *)
   (*       goCollision (kxs1, kxs2) *)

   (*      | go s (Bitmap (b1, arr1), Bitmap (b2, arr2)) = *)
   (*       goArray (go (s + bitsPerSubkey)) (b1, b2, arr1, arr2) *)
   (*      | go s (Bitmap (b1, arr1), Full arr2) = *)
   (*       goArray (go (s + bitsPerSubkey)) (b1, fullNodeMask, arr1, arr2) *)
   (*      | go s (Full arr1, Bitmap (b2, arr2)) = *)
   (*       goArray (go (s + bitsPerSubkey)) (fullNodeMask, b2, arr1, arr2) *)
   (*      | go s (Full arr1, Full arr2) = *)
   (*       goArray (go (s + bitsPerSubkey)) (fullNodeMask, fullNodeMask, arr1, arr2) *)

   (* in *)
   (*    go 0w0 *)
   (* end *)

fun intersectionWith _ = raise Fail "NYI"
fun intersection _ = raise Fail "NYI"

fun disjoint _ = raise Fail "NYI"
fun mapi _ = raise Fail "NYI"
fun map _ = raise Fail "NYI"
fun mapAccumLi _ = raise Fail "NYI"
fun mapAccumL _ = raise Fail "NYI"
fun mapAccumRi _ = raise Fail "NYI"
fun mapAccumR _ = raise Fail "NYI"
fun filteri _ = raise Fail "NYI"
fun filter _ = raise Fail "NYI"
fun mapPartiali _ = raise Fail "NYI"
fun mapPartial _ = raise Fail "NYI"
fun mapEitheri _ = raise Fail "NYI"
fun mapEither _ = raise Fail "NYI"
fun partitioni _ = raise Fail "NYI"
fun partition _ = raise Fail "NYI"
fun isSubmapBy _ = raise Fail "NYI"
fun isProperSubmapBy _ = raise Fail "NYI"
fun liftEquals _ = raise Fail "NYI"
fun collate _ = raise Fail "NYI"

end

(* vim: set tw=0 ts=3 sw=3: *)

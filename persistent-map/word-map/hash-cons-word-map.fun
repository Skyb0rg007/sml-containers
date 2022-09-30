
functor HashConsWordMapFn(S: HASH_CONS_WORD_MAP_STRUCTS) =
struct

open S

structure Tag =
   struct
      type t = word

      val counter = ref 0w0

      fun next () = !counter before counter := !counter + 0w1
   end

type key = word
type value = Value.t

datatype node =
   Nil
 | Tip of key * value
 | Bin of word * word * map * map

and map = Map of {hash: word, tag: Tag.t, node: node}

fun node (Map {node, ...}) = node
fun hash (Map {hash, ...}) = hash
fun toWord (Map {tag, ...}) = tag

fun equals (t1, t2) = toWord t1 = toWord t2

local
   fun equalsNode (Nil, Nil) = true
     | equalsNode (Tip (k, x), Tip (k', x')) = k = k' andalso Value.equals (x, x')
     | equalsNode (Bin (p, m, l, r), Bin (p', m', l', r')) =
      p = p'
      andalso m = m'
      andalso equals (l, l')
      andalso equals (r, r')
     | equalsNode _ = false

   (* Hash combining function from the Boost C++ library *)
   fun hashCombine (seed, h) =
      Word.xorb (seed, h + 0wx9e3779b9 + Word.<< (seed, 0w6) + Word.>> (seed, 0w2))

   fun hashNode Nil = 0w1
     | hashNode (Tip (k, x)) = hashCombine (0w2, hashCombine (k, Value.hash x))
     | hashNode (Bin (_, _, l, r)) = hashCombine (0w3, hashCombine (hash l, hash r))

   datatype bucketlist =
      Empty
    | Cons of map * bucketlist ref

   val tbl = ref (Array.array (32, Empty))
   val size = ref 0

   fun grow () =
      let
         val odata = !tbl
         val osize = Array.length odata
         val nsize = osize * 2
         val ndata = Array.array (nsize, Empty)
         val ndata_tail = Array.array (nsize, Empty)
         val mask = Word.fromInt (nsize - 1)

         fun insertBucket Empty = ()
           | insertBucket (cell as Cons (key, next)) =
            let
               val nidx = Word.toInt (Word.andb (hash key, mask))
            in
               case Array.sub (ndata_tail, nidx) of
                  Empty => Array.update (ndata, nidx, cell)
                | Cons (_, tail) => tail := cell
               ; Array.update (ndata_tail, nidx, cell)
               ; insertBucket (!next)
            end

      in
         Array.app insertBucket odata
         ; Array.app
           (fn Empty => ()
             | Cons (_, tail) => tail := Empty)
           ndata_tail
         ; tbl := ndata
      end
in
   fun hashcons n =
      let
         val h = hashNode n

         val data = !tbl
         val nbuckets = Array.length data
         val mask = Word.fromInt (nbuckets - 1)
         val bucketIndex = Word.toInt (Word.andb (h, mask))
         val bucket = Array.sub (data, bucketIndex)

         fun findBucket Empty = NONE
           | findBucket (cell as Cons (m, next)) =
            if h = hash m andalso equalsNode (n, node m)
               then SOME m
            else findBucket (!next)
      in
         case findBucket bucket of
            SOME m => m
          | NONE =>
               let
                  val m = Map {hash = h, node = n, tag = Tag.next ()}
               in
                  Array.update (data, bucketIndex, Cons (m, ref bucket))
                  ; size := !size + 1
                  ; if !size > nbuckets * 2 then grow () else ()
                  ; m
               end
      end
end

fun tip (k, x) = hashcons (Tip (k, x))
fun bin (p, m, l, r) = hashcons (Bin (p, m, l, r))

fun binCheckL (_, _, Map {node = Nil, ...}, r) = r
  | binCheckL (p, m, l, r) = hashcons (Bin (p, m, l, r))

fun binCheckR (_, _, l, Map {node = Nil, ...}) = l
  | binCheckR (p, m, l, r) = hashcons (Bin (p, m, l, r))

fun binCheck (_, _, l, Map {node = Nil, ...}) = l
  | binCheck (_, _, Map {node = Nil, ...}, r) = r
  | binCheck (p, m, l, r) = hashcons (Bin (p, m, l, r))

fun mask (k, m) = Word.andb (k, Word.xorb (m, Word.notb m + 0w1))

fun nomatch (k, p, m) = mask (k, m) <> p

fun zero (k, m) = Word.andb (k, m) = 0w0

fun link (p1, t1, p2, t2) =
   let
      val m = WordEx.highestBitMask (Word.xorb (p1, p2))
      val p = mask (p1, m)
   in
      if zero (p1, m)
         then bin (p, m, t1, t2)
      else bin (p, m, t2, t1)
   end

val shorter = Word.>

(** Exports **)

val empty = hashcons Nil

val singleton = tip

fun insertLookupWithi f (t, kx, x) =
   let
      fun go (Map {node = Nil, ...}) = (NONE, tip (kx, x))
        | go (Map {node = Tip (ky, y), ...}) =
         if kx = ky
            then (SOME y, tip (kx, f (kx, y, x)))
         else (NONE, link (ky, t, kx, tip (kx, x)))
        | go (Map {node = Bin (p, m, l, r), ...}) =
         if nomatch (kx, p, m)
            then (NONE, link (p, t, kx, tip (kx, x)))
         else if zero (kx, m)
            then case go l of (old, l') => (old, bin (p, m, l', r))
         else case go r of (old, r') => (old, bin (p, m, l, r'))
   in
      go t
   end

fun insertWithi f (t, kx, x) =
   let
      fun go (Map {node = Nil, ...}) = tip (kx, x)
        | go (Map {node = Tip (ky, y), ...}) =
         if kx = ky
            then tip (kx, f (kx, y, x))
         else link (ky, t, kx, tip (kx, x))
        | go (Map {node = Bin (p, m, l, r), ...}) =
         if nomatch (kx, p, m)
            then link (p, t, kx, tip (kx, x))
         else if zero (kx, m)
            then bin (p, m, go l, r)
         else bin (p, m, l, go r)
   in
      go t
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

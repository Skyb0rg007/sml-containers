
structure RRBVector =
struct

infix 7 &
infix 8 << >>

val B = 0w5
val MASK = 0w31
val BRANCHES = 32

structure A = Array
structure AS = ArraySlice
structure W = Word
structure COW = COWVectorFn(val maxLen = BRANCHES)

val () =
   if W.wordSize < 32
      then raise Fail "Word.word must be at least 32 bits"
   else ()

val op & = W.andb
val op >> = W.>>
val op << = W.<<

(* Vector Invariants:
 *
 * `size` is the number of elements in the vector
 * `shift` is a multiple of `B` representing the depth of `Leaf`s
 * `tail` is an `Empty` or `Leaf` of size `tailSize size`
 * `tree` is of size `tailOffset size`
 * `tail` is not a part of `tree`
 *
 * Tree Invariants:
 *
 * `Empty` only appears at toplevel
 *   It is also used internally as a function result to indicate failure
 * All `Leaf`s appear at the same depth
 *   This depth is always `shift / B`
 * Subchildren of `Inner` are either `Inner` or `Leaf`, never `Relaxed`
 *   These subchildren are always 'full':
 *     All but the rightmost `Inner` child have `BRANCHES` elements
 * Given `Relaxed (children, sizes)`:
 *   The length of `children` is the same as the length of `sizes`
 *   The subtree `children[i]` has `sizes[i] - sizes[i-1]` elements.
 *   (`sizes` is the cumulative sizes up to and including the index)
 *)
datatype 'a tree =
   Empty
 | Relaxed of 'a tree COW.vector * int COW.vector
 | Inner of 'a tree COW.vector
 | Leaf of 'a COW.vector

datatype 'a vector = V of {
      size : int,
      shift : W.word,
      tree : 'a tree,
      tail : 'a tree
   }

datatype 'a transient = T of COW.owner ref * 'a vector ref

(* Primitives *)

fun assert (true, _) = ()
  | assert (false, msg) = raise Fail ("Assertion failed: " ^ msg)

(* The size of the tree component of the vector,
 * AKA the offset that the first tail element represents *)
fun tailOffset 0 = 0
  | tailOffset size = W.toInt (W.fromInt (size - 1) & W.notb MASK)

(* The size of the tail component of the vector.
 * This is always between 0 and BRANCHES, and is only 0 when size is 0 *)
fun tailSize 0 = 0
  | tailSize size = W.toInt (W.fromInt (size - 1) & MASK) + 1

(* Create a single chain of `Inner`s down to a given `Leaf`
 * Used when adding a new child, since the new branch may occur before
 * the depth at which `Leaf`s must lie *)
fun makePath (_, 0w0, tail) = tail
  | makePath (e, shift, tail) =
   Inner (COW.singleton (e, makePath (e, shift - B, tail)))

(* Compute the sub-index at a given depth.
 * This is the method for fast Radix-Balanced tree lookups. *)
fun radixIndex (shift, index) =
   W.toInt ((W.fromInt index >> shift) & MASK)

(* This is the same as `radixIndex` when shift is 0.
 * Used for `Leaf` nodes since `shift` must be 0 at that point. *)
fun leafIndex index =
   W.toInt (W.fromInt index & MASK)

(* Compute the sub-index for a relaxed node.
 * Relaxed nodes' children aren't always maximally full, so looking up
 * the correct subchild is not as simple as with `radixIndex`.
 * Instead, one must search through the list of subtree sizes until
 * the correct one is found (no bit-shift optimizations).
 * Binary search is asymptotically faster than linear search, but
 * slower here since the number of subtrees is so low (≤ 32) *)
fun relaxedIndex (shift, sizes, index) =
   let
      fun go offset =
         if COW.sub (sizes, offset) <= index
            then go (offset + 1)
         else offset
   in
      go (W.toInt (W.fromInt index >> shift))
   end

(* Size of the `idx`th subchild in a relaxed node.
 * Since the `sizes` vector contains cumulative sizes, the size of a given
 * subchild is calculated by subtracting the previous entry *)
fun relaxedSize (sizes, 0) = COW.sub (sizes, 0)
  | relaxedSize (sizes, idx) = COW.sub (sizes, idx) - COW.sub (sizes, idx - 1)

(* Helpers *)

(* `e` - the owner to work with
 * `root` - the tree to insert into
 * `shift` - the depth of the tree, represented as a multiple of B
 * `size` - the size of the tree
 * `tail` - the tail node to insert (should be a `Leaf`)
 * `ts` - the size of the tail node to insert *)
fun snocTail (e, root, shift, size, tail, ts) =
   let
      val () =
         case tail of
            Leaf v => assert (COW.length v = ts, "tail size matches")
          | _ => assert (false, "tail is a Leaf")
      val () = assert (0 < ts andalso ts <= BRANCHES,
                       "tail size between 1 and BRANCHES")
      val () = assert (shift mod B = 0w0, "shift is a multiple of B")

      fun go (tree, shift, size, tail, ts) =
         case tree of
            Empty => raise Fail "impossible"
          | Leaf _ => raise Fail "impossible"
          | Inner v =>
               let
                  val idx = radixIndex (shift, size - 1)
                  val idx' = radixIndex (shift, size - 1 + BRANCHES)
               in
                  if idx = idx'
                     then
                        let
                           val oldChild = COW.sub (v, idx)
                           val newChild =
                              go (oldChild, shift - B, size, tail, ts)
                        in
                           Inner (COW.update (e, v, idx, newChild))
                        end
                  else Inner (COW.snoc (e, v, makePath (e, shift - B, tail)))
               end
          | Relaxed (v, sizes) =>
               let
                  val count = COW.length v
                  val lastIdx = count - 1
                  val lastSize = COW.sub (sizes, lastIdx)
                  val lastIsFull =
                     (* node already has the maximum number of subchildren *)
                     relaxedSize (sizes, lastIdx) = W.toInt (0w1 << shift)
                     (* child will be a `Leaf` node *)
                     orelse shift = B

                  (* Join here to add another subtree *)
                  fun addNewSubtree () =
                     let
                        val v' = COW.snoc (e, v, makePath (e, shift - B, tail))
                        val s' = COW.snoc (e, sizes, lastSize + ts)
                     in
                        Relaxed (v', s')
                     end

                  (* Join here to update the existing last child *)
                  fun updateSubtree newChild =
                     let
                        val v' = COW.update (e, v, lastIdx, newChild)
                        val s' = COW.update (e, sizes, lastIdx, lastSize + ts)
                     in
                        Relaxed (v', s')
                     end
               in
                  if count = BRANCHES andalso lastIsFull
                     (* Can't insert - last available subtree is full *)
                     then Empty
                  else if lastIsFull
                     (* Last subtree is full, but there's room for another *)
                     then addNewSubtree ()
                  else
                     (* Unsure whether there's enough room in the last child *)
                     case go (COW.sub (v, lastIdx), shift - B, size, tail, ts) of
                        Empty =>
                           (* Not enough room - add another subtree *)
                           addNewSubtree ()
                      | newChild =>
                           (* There was enough room - update the subtree *)
                           updateSubtree newChild
               end
   in
      case root of
         Empty =>
            (assert (size = 0, "Empty node has size = 0")
            ; assert (shift = 0w0, "Empty node has shift = 0w0")
            ; (0w0, tail))
       | Leaf _ => 
            (assert (size = BRANCHES, "toplevel Leaf node has size = BRANCHES")
            ; assert (shift = 0w0, "toplevel Leaf node has shift = 0w0")
            ; (B, Inner (COW.fromList (e, [root, tail]))))
       | Inner _ =>
            (* Can check ahead of time whether `go` will fail *)
            if size = W.toInt (W.fromInt BRANCHES << shift)
               then
                  let
                     val path = makePath (e, shift, tail)
                     val newRoot = Inner (COW.fromList (e, [root, path]))
                  in
                     (shift + B, newRoot)
                  end
            else (shift, go (root, shift, size, tail, ts))
       | Relaxed _ =>
            (* Can't know ahead of time - try the insertion and check for failure *)
            case go (root, shift, size, tail, ts) of
               Empty =>
                  (* Couldn't insert - increase the depth *)
                  let
                     val path = makePath (e, shift, tail)
                     val children = COW.fromList (e, [root, path])
                     val sizes = COW.fromList (e, [size, size + ts])
                  in
                     (shift + B, Relaxed (children, sizes))
                  end
             | newRoot => (shift, newRoot)
   end

(* Exported *)

val empty = V {
      size = 0,
      shift = 0w0,
      tree = Empty,
      tail = Empty
   }

fun singleton' (e, x) = V {
      size = 1,
      shift = 0w0,
      tree = Empty,
      tail = Leaf (COW.singleton (e, x))
   }

fun singleton x = singleton' (COW.noone, x)

fun snoc' (e, V {size, shift, tree, tail = Empty}, x) =
   V {
      size = size + 1,
      shift = shift,
      tree = tree,
      tail = Leaf (COW.singleton (e, x))
   }
  | snoc' (e, V {size, shift, tree, tail as Leaf v}, x) =
   let
      val ts = COW.length v
   in
      if ts < BRANCHES
         then
            V {size = size + 1,
               shift = shift,
               tree = tree,
               tail = Leaf (COW.snoc (e, v, x))}
      else
         let
            val (newShift, newRoot) = snocTail (e, tree, shift, size - ts, tail, ts)
         in
            V {size = size + 1,
               shift = newShift,
               tree = newRoot,
               tail = Leaf (COW.singleton (e, x))}
         end
   end
  | snoc' _ = raise Fail "invalid tail"

fun snoc (v, x) = snoc' (COW.noone, v, x)

fun sub (V {size, shift, tree, tail}, i) =
   if i < 0 orelse i >= size
      then raise Subscript
   else if i >= tailOffset size
      then
         case tail of
            Leaf v => COW.sub (v, leafIndex i)
          | _ => raise Fail "RRBVector.sub: invalid tail"
   else
      let
         fun go (Inner v, shift, idx) =
            go (COW.sub (v, radixIndex (shift, idx)), shift - B, idx)
           | go (Leaf v, _, idx) =
            COW.sub (v, leafIndex idx)
           | go (Relaxed (v, sizes), shift, idx) =
            let
               val offset = relaxedIndex (shift, sizes, idx)
               val child = COW.sub (v, offset)
               val idx' = relaxedSize (sizes, offset)
            in
               go (child, shift - B, idx')
            end
           | go (Empty, _, _) = raise Fail "RRBVector.sub: impossible"
      in
         go (tree, shift, i)
      end

(*
fun concat (v1, v2) =
   let
      val V {size = size1, shift = shift1, tree = tree1, tail = tail1} = v1
      val V {size = size2, shift = shift2, tree = tree2, tail = tail2} = v2
   in
      if size1 = 0
         then v2
      else if size2 = 0
         then v1
      else if tailOffset size2 = 0
         then
            let
               val tailOff = tailOffset size2
               val ts = size2 - tailOff
            in
               raise Fail "NYI"
               (* if ts = BRANCHES *)
               (*    then V { *)
               (*          size = size1 + size2, *)

               (*       } *)
            end
      else if tailOffset size1 = 0
         then raise Fail "NYI"
      else
         raise Fail "NYI"
   end

*)

structure Transient =
   struct
      type 'a transient = 'a transient

      fun transient v = T (ref (COW.newOwner ()), ref v)

      fun persistent (T (e, v)) = (e := COW.newOwner (); !v)

      val empty = fn () => T (ref (COW.newOwner ()), ref empty)

      val sub = fn (T (_, ref v), i) => sub (v, i)

      fun singleton x =
         let
            val e = COW.newOwner ()
         in
            T (ref e, ref (singleton' (e, x)))
         end

      fun snoc (T (e, v), x) =
         v := snoc' (!e, !v, x)
   end

fun fromList xs =
   let
      val t = Transient.empty ()
   in
      List.app (fn x => Transient.snoc (t, x)) xs;
      Transient.persistent t
   end

fun printTree toString (V {size, shift, tree, tail}) =
   let
      fun toStringV tos v =
         "#[" ^ COW.foldr
            (fn (x, acc) =>
               if String.size acc = 1
                  then tos x
               else tos x ^ ", " ^ acc)
            "]" v

      val tailStr =
         case tail of
            Empty => ""
          | Leaf v => toStringV toString v
          | _ => raise Fail "invalid tail"

      fun treeStr Empty = "Empty"
        | treeStr (Leaf v) = "Leaf " ^ toStringV toString v
        | treeStr (Inner v) = "Inner " ^ toStringV treeStr v
        | treeStr (Relaxed (v, sizes)) =
         String.concat
         ["Relaxed (", toStringV treeStr v, ", ",
          toStringV Int.toString sizes, ")"]
   in
      String.concat
      ["V {size = ", Int.toString size, ",\n",
       "   shift = 0wx", Word.toString shift, ",\n",
       "   tail = ", tailStr, ",\n",
       "   tree = ", treeStr tree, "}"]
   end

end

structure X : RRB_VECTOR = RRBVector

(* vim: set tw=0 ts=3 sw=3: *)

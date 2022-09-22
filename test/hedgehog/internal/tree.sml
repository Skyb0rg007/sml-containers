
structure Tree: TREE =
(* structure Tree = *)
struct

datatype 'a t = Node of 'a * 'a t Seq.t

fun root (Node (x, _)) = x
fun children (Node (_, xs)) = xs

fun unfold f x = Node (x, Seq.map (unfold f) (f x))

fun expand f (Node (x, xs)) =
   Node (x, Seq.append (Seq.map (expand f) xs, Seq.map (unfold f) (f x)))

fun prune (Node (x, xs), n) =
   if n <= 0
      then Node (x, Seq.empty)
   else Node (x, Seq.map (fn t => prune (t, n - 1)) xs)

fun pure a = Node (a, Seq.empty)

fun consChild (x, Node (root, children)) =
   Node (root, Seq.cons (pure x, children))

val extract = root

fun duplicate (n as Node (x, xs)) =
   Node (n, Seq.map duplicate xs)

fun map f (Node (x, xs)) = Node (f x, Seq.map (map f) xs)

fun extend t f = map f (duplicate t)

fun ap (Node (f, fs), a as Node (x, xs)) =
   let
      val fsn = Seq.map (fn f' => ap (f', a)) fs
      val fxs = Seq.map (map f) xs
   in
      Node (f x, Seq.append (fsn, fxs))
   end

fun map2 f (n1, n2) = ap (map (fn a => fn b => f (a, b)) n1, n2)

fun bind (Node (x, xs)) k =
   let
      val Node (y, ys) = k x
   in
      Node (y, Seq.append (Seq.map (fn x => bind x k) xs, ys))
   end

fun bindOpt (Node (x, xs)) k =
   case k x of
      NONE => NONE
    | SOME (Node (y, ys)) =>
         SOME (Node (y, Seq.append (Seq.mapPartial (fn x => bindOpt x k) xs, ys)))

fun zip (n1 as Node (x, xs), n2 as Node (y, ys)) =
   let
      val left = Seq.map (fn x => zip (x, n2)) xs
      val right = Seq.map (fn y => zip (n1, y)) ys
   in
      Node ((x, y), Seq.append (left, right))
   end

fun zipWith f (n1 as Node (x, xs), n2 as Node (y, ys)) =
   let
      val left = Seq.map (fn x => zipWith f (x, n2)) xs
      val right = Seq.map (fn y => zipWith f (n1, y)) ys
   in
      Node (f (x, y), Seq.append (left, right))
   end

fun unzip (t: ('a * 'b) t) = (map #1 t, map #2 t)

fun sequence [] = pure []
  | sequence (t :: ts) = map2 op :: (t, sequence ts)

local
   (* All ways we can remove chunks of size `k` from a list *)
   fun removes k =
      let
         fun splitAt _ [] = ([], [])
           | splitAt 1 (x :: xs) = ([x], xs)
           | splitAt n (x :: xs) =
            case splitAt (n - 1) xs of
               (xs1, xs2) => (x :: xs1, xs2)

         fun go [] () = Seq.Nil
           | go xs () =
            let
               val (xs1, xs2) = splitAt k xs
            in
               Seq.Cons (xs2, Seq.map (fn l => xs1 @ l) (go xs2))
            end
      in
         go
      end

   (* All ways we can split a list *)
   fun splits xs =
      let
         fun go (_, []) () = Seq.Nil
           | go (f, r :: rs) () =
            Seq.Cons ((List.rev f, r, rs), go (r :: f, rs))
      in
         go ([], xs)
      end

   val _: int -> 'a list -> 'a list Seq.t = removes
   val _: 'a list -> ('a list * 'a * 'a list) Seq.t = splits

   (* Shrink the list by removing batches of elements *)
   fun dropSome ts =
      let
         val ns =
            Seq.takeWhile (fn k => k > 0)
               (Seq.iterate (fn k => k div 2) (List.length ts))

         val ts' = Seq.concatMap (fn n => removes n ts) ns
      in
         Seq.map interleave ts'
      end

   (* Shrink the list by shrinking one of the elements *)
   and shrinkOne ts =
      Seq.concatMap
         (fn (xs, Node (_, ys), zs) =>
            Seq.map (fn y => interleave (xs @ [y] @ zs)) ys)
         (splits ts)
      
   and interleave ts =
      Node (List.map root ts, Seq.append (dropSome ts, shrinkOne ts))

   val _: 'a t list -> 'a list t Seq.t = dropSome
   val _: 'a t list -> 'a list t Seq.t = shrinkOne
   val _: 'a t list -> 'a list t = interleave
in
   val interleave = interleave
end

fun mapPartial f (Node (x, xs)) =
   case f x of
      NONE => NONE
    | SOME y => SOME (Node (y, Seq.mapPartial (mapPartial f) xs))

fun filter p = mapPartial (fn x => if p x then SOME x else NONE)

fun liftEquals eq (Node (x, xs), Node (y, ys)) =
   eq (x, y) andalso Seq.liftEquals (liftEquals eq) (xs, ys)

local
   val lines = String.fields (fn c => c = #"\n")

   fun shift (_, _, []) = []
     | shift (head, other, x :: xs) =
      head ^ x :: List.map (fn x => other ^ x) xs

   fun renderNode str =
      if String.size str = 1
         then " " ^ str
      else str

   fun renderTree (Node (x, xs)) =
      lines (renderNode x) @ renderForest xs

   and renderForest seq =
      case seq () of
         Seq.Nil => []
       | Seq.Cons (x, seq) =>
            case seq () of
               Seq.Nil => shift (" └╼", "   ", renderTree x)
             | Seq.Cons (y, rest) =>
                  let
                     val s = renderTree x
                     val ss = renderForest (Seq.cons (y, rest))
                  in
                     shift (" ├╼", " │ ", s) @ ss
                  end
in
   fun render t = String.concatWith "\n" (renderTree t)
end

end

(* vim: set tw=0 ts=3 sw=3: *)

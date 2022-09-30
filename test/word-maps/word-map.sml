
structure WordMapTest =
struct

structure QC = QuickCheck
structure G = QC.Generator
structure O = QC.Observer

structure M =
   struct
      open WordMap

      fun toString tos m =
         let
            fun go (k, x) =
               String.concat ["(", Word.fmt StringCvt.DEC k, ", ", tos x, ")"]
         in
            String.concat
               ["WordMap.fromList [",
                String.concatWith ", " (List.map go (toList m)),
                "]"]
         end

      fun generate gen = G.map fromList (G.listOf (G.both (G.word, gen)))
   end

structure List =
   struct
      open List

      fun sort cmp =
         let
            fun merge ([], ys) = ys
              | merge (xs, []) = xs
              | merge (x :: xs, y :: ys) =
               case cmp (x, y) of
                  LESS => x :: merge (xs, y :: ys)
                | EQUAL => x :: merge (xs, y :: ys)
                | GREATER => y :: merge (x :: xs, ys)

            fun split [] = ([], [])
              | split [x] = ([x], [])
              | split (a :: b :: xs) =
               case split xs of
                  (ys, zs) => (a :: ys, b :: zs)

            fun go [] = []
              | go [x] = [x]
              | go [x, y] = if cmp (x, y) = GREATER then [y, x] else [x, y]
              | go xs =
               case split xs of
                  (ys, zs) => merge (go ys, go zs)
         in
            go
         end

      fun sortUniq cmp =
         let
            fun merge ([], ys) = ys
              | merge (xs, []) = xs
              | merge (x :: xs, y :: ys) =
               case cmp (x, y) of
                  LESS => x :: merge (xs, y :: ys)
                | EQUAL => x :: merge (xs, ys)
                | GREATER => y :: merge (x :: xs, ys)

            fun split [] = ([], [])
              | split [x] = ([x], [])
              | split (a :: b :: xs) =
               case split xs of
                  (ys, zs) => (a :: ys, b :: zs)

            fun go [] = []
              | go [x] = [x]
              | go [x, y] = if cmp (x, y) = GREATER then [y, x] else [x, y]
              | go xs =
               case split xs of
                  (ys, zs) => merge (go ys, go zs)
         in
            go
         end
   end

fun run gen = QC.run {gen = G.resize (1000, gen), str = fn _ => "<some input>"}

fun test () =
   let
   in
      run (M.generate G.unit)
         "valid"
         M.valid

      ; run (G.both (G.word, G.int))
         "singleton"
         (fn (k, x) =>
            let
               val t = M.singleton (k, x)
            in
               M.valid t andalso M.liftEquals op = (t, M.insert (M.empty, k, x))
            end)

      ; run (G.both (G.word, M.generate G.unit))
         "insertInDomain"
         (fn (k, t) => M.inDomain (M.insert (t, k, ()), k))

      ; run (G.both (G.word, M.generate G.unit))
         "insertDelete"
         (fn (k, t) =>
            case M.find (t, k) of
               SOME _ => true
             | NONE =>
                  let
                     val t' = M.delete (M.insert (t, k, ()), k)
                  in
                     M.valid t' andalso M.liftEquals op = (t, t')
                  end)

      ; run (G.both (G.word, M.generate G.unit))
         "deleteNonMember"
         (fn (k, t) =>
            M.inDomain (t, k)
            orelse M.liftEquals op = (t, M.delete (t, k)))

      ; run (G.both (M.generate G.int, G.both (G.word, G.int)))
         "unionSingleton"
         (fn (t, (k, x)) =>
            M.liftEquals op = (M.unionWith #1 (M.singleton (k, x), t), M.insert (t, k, x)))

      ; run (G.both (M.generate G.int, G.both (M.generate G.int, M.generate G.int)))
         "unionAssoc"
         (fn (t1, (t2, t3)) =>
            M.liftEquals op =
               (M.unionWith #1 (t1, M.unionWith #1 (t2, t3)),
                M.unionWith #1 (M.unionWith #1 (t1, t2), t3)))

      ; run (G.both (G.listOf (G.both (G.word, G.word)), G.listOf (G.both (G.word, G.word))))
         "unionSum"
         (fn (xs, ys) =>
            let
               val sum = List.foldl op + 0w0
            in
               sum (M.elems (M.unionWith op + (M.fromListWith op + xs, M.fromListWith op + ys)))
               = sum (List.map #2 xs) + sum (List.map #2 ys)
            end)

(*       ; run (G.both (G.listOf (G.both (G.word, G.word)), G.listOf (G.both (G.word, G.word)))) *)
(*          "differenceModel" *)
(*          (fn (xs, ys) => *)
(*             let *)
(*                val t = M.difference (M.fromListWith op + xs, M.fromListWith op + ys) *)
(*                fun nub *) 
(*             in *)
(*             end) *)

      ; run (G.both (M.generate G.unit, M.generate G.unit))
         "disjoint"
         (fn (m1, m2) =>
            M.disjoint (m1, m2) = M.isEmpty (M.intersection (m1, m2)))

      ; run (G.listOf G.word)
         "list"
         (fn xs =>
            List.sortUniq Word.compare xs
            = M.keys (M.fromList (List.map (fn x => (x, ())) xs)))
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

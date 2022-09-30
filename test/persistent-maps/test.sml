
functor PMTestFn(
   structure Map: MONO_MAP
   val genKey: Map.key Hedgehog.Gen.t
   val keyToString: Map.key -> string

   val genVal: Map.value Hedgehog.Gen.t
   val valToString: Map.value -> string
   val valEquals: Map.value * Map.value -> bool) =
   struct
      open Hedgehog

      infixr 9 $
      fun f $ x = f x

      val genMap =
         Gen.map
         Map.fromList
         (Gen.list (Range.Int.linearFrom {origin = 0, lo = 0, hi = 500})
            (Gen.map2 (fn p => p) genKey genVal))

      val equals = Map.liftEquals valEquals

      fun mapToString m =
         "{" ^ String.concatWith ","
         (List.map (fn (k, x) => ("(" ^ keyToString k ^ "," ^ valToString x ^ ")")) (Map.toList m))
         ^ "}"

      fun assert b = if b then Test.success else Test.failure

      val () = check $ property $
         Test.bind (Test.forallWith keyToString genKey) (fn k =>
         Test.bind (Test.forallWith valToString genVal) (fn v =>
         assert (equals (Map.singleton (k, v), Map.insert (Map.empty, k, v)))))

      val () = check $ property $
         Test.bind (Test.forallWith mapToString genMap) (fn m =>
         Test.bind (Test.forallWith keyToString genKey) (fn k =>
         Test.bind (Test.forallWith valToString genVal) (fn v =>
         assert (Map.inDomain (Map.insert (m, k, v), k)))))
         (* if equals (Map.singleton (k, v), Map.insert (Map.empty, k, v)) *)
         (*    then Test.success *)
         (* else Test.failure)) *)
   end

structure PMTest =
struct
   open Hedgehog

   structure X = PMTestFn(
      struct
         structure Map = MonomorphizeMapFn(
            struct
               structure Map = WordMap
               type value = int
            end)
         val genKey = Gen.word (Range.Word.linearFrom {origin = 0w0, lo = 0w0, hi = Word.notb 0w0})
         val genVal = Gen.int (Range.Int.linearFrom {origin = 0, lo = 0, hi = 100})
         val keyToString = Word.fmt StringCvt.DEC
         val valToString = Int.toString
         val valEquals: int * int -> bool = op =
      end)

   structure X = PMTestFn(
      struct
         structure Map = LSSWordMapFn(
            struct
               structure Value = 
                  struct
                     type t = int
                     val equals: t * t -> bool = op =
                  end

               structure Aug =
                  struct
                     type t = word

                     val zero = 0wxdeadbeef
                     fun pure (k, x) = Word.xorb (k, Word.fromInt x)
                     val equals: t * t -> bool = op =
                     val op + = Word.xorb
                  end
            end)
         val genKey = Gen.word Range.Word.linear
         val genVal = Gen.int (Range.Int.linearFrom {origin = 0, lo = 0, hi = 100})
         val keyToString = Word.fmt StringCvt.DEC
         val valToString = Int.toString
         val valEquals: int * int -> bool = op =
      end)
end

(* vim: set tw=0 ts=3 sw=3: *)

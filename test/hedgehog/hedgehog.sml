
structure Hedgehog: HEDGEHOG =
struct

structure Range = Range
structure Gen = Gen
structure Test = Test

datatype config = Config of {
      discardLimit: int,
      shrinkLimit: int,
      shrinkRetries: int,
      minTests: int
   }

val defaultConfig = Config {
      discardLimit = 100,
      shrinkLimit = 1000,
      shrinkRetries = 0,
      minTests = 1000
   }

datatype property = Property of config * unit Test.t

fun property t = Property (defaultConfig, t)

datatype result =
   Ok
 | GaveUp
 | Failed of string * Test.journal

fun printResult Ok = TextIO.print "passed\n"
  | printResult GaveUp = TextIO.print "gave up\n"
  | printResult (Failed (e, j)) =
   (TextIO.print "failed\n"
    ; List.app
      (fn Test.Annotation ann => TextIO.print (ann ^ "\n")
        | _ => ())
      j
    ; List.app
      (fn Test.Footnote msg => TextIO.print (msg ^ "\n")
        | _ => ())
      j)

fun takeSmallest (slimit, retries, tree) =
   let
      fun loop (shrinks, t) =
         case Tree.root t of
            (Test.Ok _, _) => GaveUp
          | (Test.Err e, logs) =>
            if shrinks >= slimit
               then Failed (e, logs)
            else loopChildren (shrinks, Failed (e, logs), Tree.children t)

      and loopChildren (shrinks, failed, seq) =
         case seq () of
            Seq.Nil => failed
          | Seq.Cons (t, seq') =>
               case Tree.root t of
                  (Test.Err _, _) => loop (shrinks + 1, t)
                | (Test.Ok _, _) => loopChildren (shrinks, failed, seq')
   in
      loop (0, tree)
   end

fun checkProperty (Property (conf, test)) =
   let
      val Config {discardLimit, shrinkLimit, shrinkRetries, minTests} = conf

      fun loop (tests, discards, size, seed) =
         if size > 99
            then loop (tests, discards, 0, seed)
         else if tests >= minTests
            then Ok
         else if discards >= discardLimit
            then GaveUp
         else
            let
               val (s1, s2) = Seed.split seed
            in
               case Gen.run (test, size, s1) of
                  NONE => loop (tests, discards + 1, size + 1, s2)
                | SOME t =>
                     case Tree.root t of
                        (Test.Ok (), journal) =>
                           loop (tests + 1, discards, size, s2)
                      | (Test.Err _, _) =>
                           takeSmallest (shrinkLimit, shrinkRetries, t)
            end

      val size = 30
      val seed = Seed.new ()
   in
      loop (0, 0, size, seed)
   end

fun check prop = printResult (checkProperty prop)

end

(* vim: set tw=0 ts=3 sw=3: *)

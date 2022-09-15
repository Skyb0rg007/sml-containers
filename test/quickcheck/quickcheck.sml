
structure QuickCheck: QUICKCHECK =
struct

structure SplitMix = SplitMix
structure Generator = Generator
structure Observer = Observer

type 'a arbitrary = {
   gen: 'a Generator.t,
   str: 'a -> string
   (* TODO: Shrinking *)
}

val num_tests = ref 100
val size = 30
val rng = SplitMix.new ()

exception TestFailure of string

fun fail name input =
   let
      val msg = String.concat
         ["[Test Failure]: ", name, ": on input ", input, "\n"]
   in
      TextIO.output (TextIO.stdErr, msg)
      ; raise TestFailure name
   end

fun failExn name exn input =
   let
      val msg = String.concat
         ["[Test Failure]: ", name,
          " with exception ", exnName exn,
          " on input ", input, "\n"]
   in
      TextIO.output (TextIO.stdErr, msg)
      ; raise TestFailure name
   end

fun run {gen, str} name f =
   let
      fun go 0 = ()
        | go n =
         let
            val x = Generator.generate (gen, size, rng)
         in
            (if f x then go (n + 1) else fail name (str x))
            handle exn => failExn name exn (str x)
         end
   in
      go (!num_tests)
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

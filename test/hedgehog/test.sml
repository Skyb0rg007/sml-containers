
structure Test =
struct

datatype cover = NoCover | Cover

structure Label:
   sig
      type 'a t

      val name: 'a t -> string
      val minimum: 'a t -> real
      val annotation: 'a t -> 'a
      val covered: int t * int -> bool
      val bounds: {label: int t, tests: int, confidence: IntInf.int} -> real * real
   end =
   struct
      datatype 'a t = Label of {
         name: string,
         minimum: real,
         annotation: 'a
      }

      fun name (Label {name, ...}) = name
      fun minimum (Label {minimum, ...}) = minimum
      fun annotation (Label {annotation, ...}) = annotation

      fun covered (lbl, tests) =
         let
            val count = annotation lbl
            val percentage = Real.fromInt count / Real.fromInt tests * 100.0
            val thousandths = Real.realRound (percentage * 10.0)
         in
            thousandths / 10.0 >= minimum lbl
         end

      fun bounds {tests, confidence, label} =
         let
            val positives = Int.toLarge (annotation label)
            val count = Int.toLarge tests
            val acceptance = 1.0 / Real.fromLargeInt confidence

            val n = Real.fromLargeInt count
            (* val z = invnormcdf ((1.0 - acceptance) / 2.0) *)
         in
            (0.0, 0.0)
         end

   end

type 'a label = {
      name: string,
      minimum: real,
      annotation: 'a
   }

datatype log =
   Annotation of string
 | Footnote of string
 | Label of cover label

type journal = log list

datatype 'a result =
   Ok of 'a
 | Err of string

type 'a t = ('a result * journal) Gen.t

fun run (g, size, seed) = Gen.run (g, size, seed)

structure M =
   MonadFn(
      struct
         type 'a t = 'a t

         fun pure a = Gen.pure (Ok a, [])
         fun map f = Gen.map (fn (Ok a, j) => (Ok (f a), j) | (Err e, j) => (Err e, j))
         fun bind m k =
            Gen.bind m
            (fn (Err e, j) => Gen.pure (Err e, j)
              | (Ok a, j) => Gen.map (fn (b, j') => (b, j @ j')) (k a))
      end)

open M

val discard = Gen.discard

val success = pure ()

fun failWith mdiff msg = Gen.pure (Err msg, [])
fun fail msg = Gen.pure (Err msg, [])

val failure = Gen.T (fn _ => SOME (Tree.pure (Err "", [])))

fun forallWith render = Gen.map (fn a => (Ok a, [Annotation (render a)]))

fun tell w = Gen.pure (Ok (), w)

fun annotate msg = tell [Annotation msg]
fun footnote msg = tell [Footnote msg]

end

(* vim: set tw=0 ts=3 sw=3: *)

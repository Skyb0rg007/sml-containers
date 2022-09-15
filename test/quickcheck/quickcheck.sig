
signature QUICKCHECK =
sig

structure SplitMix: SPLITMIX
structure Generator: GENERATOR
structure Observer: OBSERVER
   where type 'a t = 'a Generator.observer
     and type 'a generator = 'a Generator.t

type 'a arbitrary = {
   gen: 'a Generator.t,
   str: 'a -> string
   (* TODO: Shrinking *)
}

exception TestFailure of string

val run: 'a arbitrary -> string -> ('a -> bool) -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)


signature HASH_CONS_WORD_MAP_STRUCTS =
   sig
      structure Value:
         sig
            type t

            val equals: t * t -> bool
            val hash: t -> word
         end
   end

signature HASH_CONS_WORD_MAP =
   sig
      include MONO_MAP where type key = word

      val viewMin: map -> (key * value * map) option
      val viewMax: map -> (key * value * map) option
      val equals: map * map -> bool

      (* This is an identity function, i.e.
       * `toWord m1 = toWord m2` if and only if `equals (m1, m2)` *)
      val toWord: map -> word
   end

(* vim: set tw=0 ts=3 sw=3: *)

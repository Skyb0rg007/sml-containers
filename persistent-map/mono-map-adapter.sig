
signature MONO_MAP_ADAPTER_STRUCTS =
sig

type key
type value

structure Map: MONO_MAP where type value = key * value

structure Key:
   sig
      type t = key

      val adapt: t -> Map.key
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

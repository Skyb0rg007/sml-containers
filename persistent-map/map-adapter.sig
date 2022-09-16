
signature MAP_ADAPTER_STRUCTS =
sig
   structure Map: MAP
   structure Key:
      sig
         type t

         val adapt: t -> Map.key
      end
end

(* vim: set tw=0 ts=3 sw=3: *)

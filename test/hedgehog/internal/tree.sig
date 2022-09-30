
signature TREE =
sig

type 'a t

(* Construct a tree from a root and child sequence *)
val node: 'a * 'a t Seq.t -> 'a t

(* Access the root node *)
val root: 'a t -> 'a

(* Access the tree's children *)
val children: 'a t -> 'a t Seq.t

(* Grow the tree by applying the given shrink function to each tree level *)
val expand: ('a -> 'a Seq.t) -> 'a t -> 'a t

(* Cut a tree so its depth is limited *)
val prune: 'a t * int -> 'a t

(* Prepend an element to the children of the given tree *)
val consChild: 'a * 'a t -> 'a t

(* Map a function over the tree *)
val map: ('a -> 'b) -> 'a t -> 'b t

(* Create a singleton tree *)
val pure: 'a -> 'a t

(* Zip together two trees, combining nodes with the same path with the function *)
val zipWith: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t

(* Monadic bind, with the possibility for failure *)
val bindOpt: 'a t -> ('a -> 'b t option) -> 'b t option

(* Generate possible shrinks for a list of trees *)
val interleave: 'a t list -> 'a list t

(* Map over a tree, removing a node if the function returns NONE *)
val mapPartial: ('a -> 'b option) -> 'a t -> 'b t option

(* Convert a tree into a nice printable string *)
val render: string t -> string

end

(* vim: set tw=0 ts=3 sw=3: *)

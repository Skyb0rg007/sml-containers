
signature TREE =
sig

datatype 'a t = Node of 'a * 'a t Seq.t

(* Access the root node *)
val root: 'a t -> 'a

(* Access the tree's children *)
val children: 'a t -> 'a t Seq.t

(* Build a tree through succesive applications a shrinking function *)
val unfold: ('a -> 'a Seq.t) -> 'a -> 'a t

(* Grow the tree by applying the given shrink function to each tree level *)
val expand: ('a -> 'a Seq.t) -> 'a t -> 'a t

(* Cut a tree so its depth is limited *)
val prune: 'a t * int -> 'a t

(* Prepend an element to the children of the given tree *)
val consChild: 'a * 'a t -> 'a t

(** Tree is a Functor **)

(* Map a function over the tree *)
val map: ('a -> 'b) -> 'a t -> 'b t

(** Tree is an Applicative Functor **)

(* Create a singleton tree *)
val pure: 'a -> 'a t

(* Combine the trees by merging every combination of subtrees in order *)
val ap: ('a -> 'b) t * 'a t -> 'b t

(* Map over two trees, resulting in a result of combinations *)
val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t

(** Tree is a Monad **)

(* Expand each element into a subtree *)
val bind: 'a t -> ('a -> 'b t) -> 'b t

(* Create a tree filled with lists of elements, one from each given tree *)
val sequence: 'a t list -> 'a list t

(** Tree is a Comonad **)

(* Extract the root node *)
val extract: 'a t -> 'a

(* A tree containing all its subtrees *)
val duplicate: 'a t -> 'a t t

(* Construct a tree by applying a function to every subtree *)
val extend: 'a t -> ('a t -> 'b) -> 'b t

(** More exotic combinators **)
val zip: 'a t * 'b t -> ('a * 'b) t
val zipWith: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
val unzip: ('a * 'b) t -> 'a t * 'b t
val bindOpt: 'a t -> ('a -> 'b t option) -> 'b t option
val interleave: 'a t list -> 'a list t

(** Partial functions **)
val filter: ('a -> bool) -> 'a t -> 'a t option
val mapPartial: ('a -> 'b option) -> 'a t -> 'b t option

(** Useful operations **)
val liftEquals: ('a * 'b -> bool) -> 'a t * 'b t -> bool
val render: string t -> string

end

(* vim: set tw=0 ts=3 sw=3: *)

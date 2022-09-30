(* Common monad combinators *)

signature MONAD =
sig

type 'a t

(* Inject a value into the monadic type *)
val pure: 'a -> 'a t
(* Apply a function to the monadic value *)
val map: ('a -> 'b) -> 'a t -> 'b t
(* Map a binary function over two monadic actions *)
val map2: ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
(* Map a ternary function over two monadic actions *)
val map3: ('a * 'b * 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(* Sequential application *)
val ap: ('a -> 'b) t -> 'a t -> 'b t
(* Sequence two actions, passing the result of the first to the second *)
val bind: 'a t -> ('a -> 'b t) -> 'b t
(* Remove a level of monadic structure *)
val join: 'a t t -> 'a t
(* Map each element of the list to a monadic action, evaluating left-to-right *)
val mapM: ('a -> 'b t) -> 'a list -> 'b list t
(* `mapM` with its arguments flipped *)
val forM: 'a list -> ('a -> 'b t) -> 'b list t
(* Evaluate each monadic action in the list from left-to-right *)
val sequence: 'a t list -> 'a list t
(* Perform a monadic action a given number of times *)
val replicateM: int -> 'a t -> 'a list t
(* Versions of above functions, but ignoring the results *)
val mapM_: ('a -> 'b t) -> 'a list -> unit t
val forM_: 'a list -> ('a -> 'b t) -> unit t
val sequence_: 'a t list -> unit t
val replicateM_: int -> 'a t -> unit t

(** Infix operators **)

(* Infix version of `map` (infix 4) *)
val <$> : ('a -> 'b) * 'a t -> 'b t
(* Flipped version of `<$>` (infix 1) *)
val <&> : 'a t * ('a -> 'b) -> 'b t
(* Alias for `map o Fn.const` (infix 4) *)
val <$ : 'a * 'b t -> 'a t
(* Flipped version `<$` (infix 4) *)
val $> : 'a t * 'b -> 'b t
(* Infix version of `ap` (infix 4) *)
val <*> : ('a -> 'b) t * 'a t -> 'b t
(* Flipped variant of `<*>`. Still performs first argument first. (infix 4) *)
val <**> : 'a t * ('a -> 'b) t -> 'b t
(* Sequence actions, discarding value from first argument (infix 4) *)
val *> : 'a t * 'b t -> 'b t
(* Sequence actions, discarding value from second argument (infix 4) *)
val <* : 'a t * 'b t -> 'a t
(* Infix version of `bind` (infix 1) *)
val >>= : 'a t * ('a -> 'b t) -> 'b t
(* Flipped version of `>>=` (infixr 1) *)
val =<< : ('a -> 'b t) * 'a t -> 'b t
(* Left-to-right Kleisli composition (infixr 1) *)
val >=> : ('a -> 'b t) * ('b -> 'c t) -> 'a -> 'c t
(* Right-to-left Kleisli composition (infixr 1) *)
val <=< : ('b -> 'c t) * ('a -> 'b t) -> 'a -> 'c t

end

(* vim: set tw=0 ts=3 sw=3: *)


signature MONAD_BASE =
   sig
      type 'a t

      val pure: 'a -> 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val bind: 'a t -> ('a -> 'b t) -> 'b t
   end

functor MonadFn(M: MONAD_BASE) :> MONAD where type 'a t = 'a M.t =
   struct
      open M

      infix 1 <&> >>=
      infixr 1 =<< >=> <=<
      infix 4 <$> <$ $> <*> <**> *> <*

      fun f <$> m = map f m
      fun m <&> f = map f m
      fun x <$ m = map (fn _ => x) m
      fun m $> x = map (fn _ => x) m

      fun ap m1 m2 = bind m1 (fn x1 => map (fn x2 => x1 x2) m2)
      fun m1 <*> m2 = ap m1 m2
      fun m1 <**> m2 = bind m1 (fn x1 => map (fn x2 => x2 x1) m2)
      fun m1 *> m2 = bind m1 (fn _ => m2)
      fun m1 <* m2 = bind m1 (fn x => x <$ m2)
      fun map2 f m1 m2 = bind m1 (fn x1 => map (fn x2 => f (x1, x2)) m2)
      fun map3 f m1 m2 m3 =
         bind m1 (fn x1 =>
         bind m2 (fn x2 =>
         map (fn x3 => f (x1, x2, x3)) m3))

      fun m >>= k = bind m k
      fun k =<< m = bind m k
      fun f >=> g = fn x => bind (f x) g
      fun g <=< f = f >=> g
      fun join m = bind m (fn x => x)
      (* TODO: Tail recursion? *)
      fun mapM _ [] = pure []
        | mapM f (m :: ms) = map2 op :: (f m) (mapM f ms)
      fun forM ms f = mapM f ms
      fun sequence [] = pure []
        | sequence (m :: ms) = map2 op :: m (sequence ms)
      fun replicateM n m =
         if n <= 0
            then pure []
         else map2 op :: m (replicateM (n - 1) m)
      fun replicateM_ n m =
         if n <= 0
            then pure ()
         else m *> replicateM_ (n - 1) m
      fun mapM_ _ [] = pure ()
        | mapM_ f (m :: ms) = f m *> mapM_ f ms
      fun forM_ ms f = mapM_ f ms
      fun sequence_ [] = pure ()
        | sequence_ (m :: ms) = m *> sequence_ ms
   end

(* vim: set tw=0 ts=3 sw=3: *)

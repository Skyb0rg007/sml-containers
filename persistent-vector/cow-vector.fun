
functor COWVectorFn(val maxLen : int) :> COW_VECTOR =
struct

type owner = unit ref

datatype 'a vector = Cow of owner * int ref * 'a Array.array

val noone : owner = ref ()

fun newOwner () = ref ()

val maxLen = maxLen

fun check (Cow (e, ref len, arr)) =
   if len < 0
      then raise Fail "invalid length"
   else if len > Array.length arr
      then raise Fail "length > capacity"
   else if e = noone andalso len <> Array.length arr
      then raise Fail "unowned vector has length < capacity"
   else if e <> noone andalso Array.length arr <> maxLen
      then raise Fail "owned vector has capacity <> maxLen"
   else ()

fun length (Cow (_, ref len, _)) = len

fun sub (Cow (_, ref len, arr), i) =
   if i >= len
      then raise Subscript
   else Array.sub (arr, i)

fun toList (Cow (_, ref len, arr)) =
   List.tabulate (len, fn i => Array.sub (arr, i))

fun foldli f z (Cow (_, ref len, arr)) =
   ArraySlice.foldli f z (ArraySlice.slice (arr, 0, SOME len))

fun foldl f = foldli (fn (_, x, b) => f (x, b))

fun foldri f z (Cow (_, ref len, arr)) =
   ArraySlice.foldri f z (ArraySlice.slice (arr, 0, SOME len))

fun foldr f = foldri (fn (_, x, b) => f (x, b))

fun appi f (Cow (_, ref len, arr)) =
   ArraySlice.appi f (ArraySlice.slice (arr, 0, SOME len))

fun app f (Cow (_, ref len, arr)) =
   ArraySlice.app f (ArraySlice.slice (arr, 0, SOME len))

fun all p (Cow (_, ref len, arr)) =
   ArraySlice.all p (ArraySlice.slice (arr, 0, SOME len))

fun exists p (Cow (_, ref len, arr)) =
   ArraySlice.exists p (ArraySlice.slice (arr, 0, SOME len))

fun collate cmp (Cow (_, ref len1, arr1), Cow (_, ref len2, arr2)) =
   let
      fun go i =
         case (i >= len1, i >= len2) of
            (true, true) => EQUAL
          | (true, false) => LESS
          | (false, true) => GREATER
          | (false, false) =>
               case cmp (Array.sub (arr1, i), Array.sub (arr2, i)) of
                  EQUAL => go (i + 1)
                | order => order
   in
      go 0
   end

fun ptrEqual (Cow (_, len1, _), Cow (_, len2, _)) = len1 = len2

fun singleton (e, x) =
   if e = noone
      then Cow (noone, ref 1, Array.array (1, x))
   else
      let
         val arr = ArrayEx.alloc maxLen
      in
         Array.update (arr, 0, x);
         Cow (e, ref 1, arr)
      end

fun fromList (e, xs) =
   if e = noone
      then
         let
            val arr = Array.fromList xs
         in
            Cow (noone, ref (Array.length arr), arr)
         end
   else
      let
         val arr = ArrayEx.alloc maxLen
         val len = ref 0
         fun go (_, []) = ()
           | go (i, x :: xs) =
            (Array.update (arr, i, x);
             len := !len + 1;
             go (i + 1, xs))
      in
         go (0, xs);
         Cow (e, len, arr)
      end

fun update (e, cow as Cow (e', ref len, arr), i, x) =
   if i >= len
      then raise Subscript
   else if e = noone
      then
         let
            val arr' = ArrayEx.alloc len
            val slice = ArraySlice.slice (arr, 0, SOME len)
         in
            ArraySlice.copy {di = 0, dst = arr', src = slice};
            Array.update (arr', i, x);
            Cow (noone, ref len, arr')
         end
   else if e = e'
      then (Array.update (arr, i, x); cow)
   else
      let
         val arr' = ArrayEx.alloc maxLen
      in
         Array.copy {di = 0, dst = arr', src = arr};
         Array.update (arr', i, x);
         Cow (e, ref len, arr')
      end

fun snoc (e, cow as Cow (e', len, arr), x) =
   if !len = maxLen
      then raise Domain
   else if e = noone
      then
         let
            val arr' = ArrayEx.alloc (!len + 1)
            val slice = ArraySlice.slice (arr, 0, SOME (!len))
         in
            ArraySlice.copy {di = 0, dst = arr', src = slice};
            Array.update (arr', !len, x);
            Cow (noone, ref (!len + 1), arr')
         end
   else if e = e'
      then (Array.update (arr, !len, x); len := !len + 1; cow)
   else
      let
         val arr' = ArrayEx.alloc maxLen
      in
         Array.copy {di = 0, dst = arr', src = arr};
         Array.update (arr', !len, x);
         Cow (e, ref (!len + 1), arr')
      end

fun take (e, cow as Cow (e', len, arr), n) =
   if n < 0 orelse n > !len
      then raise Subscript
   else if e = noone
      then
         if e' = noone andalso n = !len
            then cow
         else 
            let
               val arr' = ArrayEx.alloc n
               val slice = ArraySlice.slice (arr, 0, SOME n)
            in
               ArraySlice.copy {di = 0, dst = arr', src = slice};
               Cow (noone, ref n, arr')
            end
   else if e = e'
      then
         (while !len > n do (
             len := !len - 1;
             ArrayEx.uninit (arr, !len));
          cow)
   else
      let
         val arr' = ArrayEx.alloc maxLen
      in
         Array.copy {di = 0, dst = arr', src = arr};
         Cow (e, ref n, arr')
      end

fun drop (e, cow as Cow (e', len, arr), n) =
   if n < 0 orelse n > !len
      then raise Subscript
   else if e = noone
      then
         if n = 0
            then cow
         else
            let
               val newLen = !len - n
               val arr' = ArrayEx.alloc newLen
               val slice = ArraySlice.slice (arr, n, SOME newLen)
            in
               ArraySlice.copy {di = 0, dst = arr', src = slice};
               Cow (noone, ref newLen, arr')
            end
   else
      let
         val oldLen = !len
         val newLen = oldLen - n
         val slice = ArraySlice.slice (arr, n, SOME newLen)
      in
         if e = e'
            then
               let
                  fun clear i =
                     if i < oldLen
                        then (ArrayEx.uninit (arr, i); clear (i + 1))
                     else ()
               in
                  ArraySlice.copy {di = 0, dst = arr, src = slice};
                  clear newLen;
                  len := newLen;
                  cow
               end
         else
            let
               val arr' = ArrayEx.alloc maxLen
            in
               ArraySlice.copy {di = 0, dst = arr', src = slice};
               Cow (e, ref newLen, arr')
            end
      end

fun append (e, cow1 as Cow (e1, len1, arr1), cow2 as Cow (e2, len2, arr2)) =
   if !len1 + !len2 > maxLen
      then raise Domain
   else if e = noone
      then
         if e1 = noone andalso !len2 = 0
            then cow1
         else if e2 = noone andalso !len1 = 0
            then cow2
         else
            let
               val arr' = ArrayEx.alloc (!len1 + !len2)
               val slice1 = ArraySlice.slice (arr1, 0, SOME (!len1))
               val slice2 = ArraySlice.slice (arr2, 0, SOME (!len2))
            in
               ArraySlice.copy {di = 0, dst = arr', src = slice1};
               ArraySlice.copy {di = !len1, dst = arr', src = slice2};
               Cow (noone, ref (!len1 + !len2), arr')
            end
   else if e = e1
      then
         let
            val slice2 = ArraySlice.slice (arr2, 0, SOME (!len2))
         in
            ArraySlice.copy {di = !len1, dst = arr1, src = slice2};
            len1 := !len1 + !len2;
            cow1
         end
   else if e = e2
      then
         let
            val slice1 = ArraySlice.slice (arr1, 0, SOME (!len1))
            val slice2 = ArraySlice.slice (arr2, 0, SOME (!len2))
         in
            ArraySlice.copy {di = !len1, dst = arr2, src = slice2};
            ArraySlice.copy {di = 0, dst = arr1, src = slice1};
            len2 := !len1 + !len2;
            cow2
         end
   else
      let
         val arr' = ArrayEx.alloc maxLen
         val slice1 = ArraySlice.slice (arr1, 0, SOME (!len1))
         val slice2 = ArraySlice.slice (arr2, 0, SOME (!len2))
      in
         ArraySlice.copy {di = 0, dst = arr', src = slice1};
         ArraySlice.copy {di = !len1, dst = arr', src = slice2};
         Cow (e, ref (!len1 + !len2), arr')
      end

end

(* vim: set tw=0 ts=3 sw=3: *)

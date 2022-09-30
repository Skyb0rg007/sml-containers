
structure Score =
struct

(* Code taken from CPython stdlib *)
fun inv_cdf p =
   if p <= 0.0 orelse p >= 1.0
      then raise Domain
   else
      let
         val q = p - 0.5
      in
         if Real.abs q <= 4.25
            then
               let
                  val r = 0.180625 - q * q
                  val num = (((((((2.5090809287301226727e3 * r + 3.3430575583588128105e4) * r + 6.7265770927008700853e4) * r + 4.5921953931549871457e4) * r + 1.3731693765509461125e4) * r + 1.9715909503065514427e3) * r + 1.3314166789178437745e2) * r + 3.3871328727963666080e0) * q
                  val den = ((((((5.2264952788528545610e3 * r + 2.8729085735721942674e4) * r + 3.9307895800092710610e4) * r + 2.1213794301586595867e4) * r + 5.3941960214247511077e3) * r + 6.8718700749205790830e2) * r + 4.2313330701600911252e1) * r + 1.0
               in
                  if Real.== (den, 0.0)
                     then raise Domain
                  else num / den
               end
         else
            let
               val r = if q <= 0.0 then p else 1.0 - p
               val () =
                  if r < 0.0 orelse r >= 1.0
                     then raise Domain
                  else ()

               val r = Math.sqrt (~(Math.ln r))
            in
               if r <= 5.0
                  then
                     let
                        val r = r - 1.6
                        val num = ((((((7.74545014278341407640e~4 * r + 2.27238449892691845833e~2) * r + 2.41780725177450611770e~1) * r + 1.27045825245236838258e0) * r + 3.64784832476320460504e0) * r + 5.76949722146069140550e0) * r + 4.63033784615654529590e0) * r + 1.42343711074968357734e0
                        val den = ((((((1.05075007164441684324e~9 * r + 5.47593808499534494600e~4) * r + 1.51986665636164571966e~2) * r + 1.48103976427480074590e~1) * r + 6.89767334985100004550e~1) * r + 1.67638483018380384940e0) * r + 2.05319162663775882187e0) * r + 1.0
                     in
                        if Real.== (den, 0.0)
                           then raise Domain
                        else num / den
                     end
               else
                  let
                     val r = r - 5.0
                     val num = ((((((2.01033439929228813265e~7 * r + 2.71155556874348757815e~5) * r + 1.24266094738807843860e~3) * r + 2.65321895265761230930e~2) * r + 2.96560571828504891230e~1) * r + 1.78482653991729133580e0) * r + 5.46378491116411436990e0) * r + 6.65790464350110377720e0
                     val den = ((((((2.04426310338993978564e~15 * r + 1.42151175831644588870e~7) * r + 1.84631831751005468180e~5) * r + 7.86869131145613259100e~4) * r + 1.48753612908506148525e~2) * r + 1.36929880922735805310e~1) * r + 5.99832206555887937690e~1) * r + 1.0
                  in
                     if Real.== (den, 0.0)
                        then raise Domain
                     else num / den
                  end
            end
      end

fun wilsonBounds {count = 0, ...} = raise Domain
  | wilsonBounds {positives, count, confidence} =
   let
      val p = Real.fromLargeInt positives
      val n = Real.fromLargeInt count
      val acceptance = 1.0 / (Real.fromLargeInt confidence)
      val p_hat = 1.0 * p / n
      val z = inv_cdf (1.0 - acceptance / 2.0)
      val midpoint = p_hat + z * z / (2.0 * n)
      val denominator = 1.0 + z * z / n
      val offset = z / (1.0 + z * z / n) * Math.sqrt (p_hat * (1.0 - p_hat) / n + z * z / (4.0 * n * n))

      val low = (midpoint - offset) / denominator
      val high = (midpoint + offset) / denominator
   in
      (low, high)
   end

end

(* vim: set tw=0 ts=3 sw=3: *)

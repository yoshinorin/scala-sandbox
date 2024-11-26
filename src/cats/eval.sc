// https://typelevel.org/cats/datatypes/eval.html
//> using dep "org.typelevel::cats-core:2.12.0"

import cats.Eval
import cats.syntax.all.*

/** \=============================================== Eager
  */
val eager = Eval.now {
  println("Running expensive calculation...")
  1 + 2 + 3
}
// Running expensive calculation...
// eager: Eval[Int] = Now(value = 7)

eager.value
// res0: Int = 7

/** \=============================================== Lazy
  */
val lazyEval = Eval.later {
  println("Running expensive calculation...")
  1 + 2 * 3
}

lazyEval.value
// Running expensive calculation...
// res1: Int = 7

lazyEval.value
// res2: Int = 7

/** \=============================================== Always
  */
val always = Eval.always {
  println("Running expensive calculation...")
  1 + 2 * 3
}
// always: Eval[Int] = cats.Always@57f3410a

always.value
// Running expensive calculation...
// res3: Int = 7

always.value
// Running expensive calculation...
// res4: Int = 7

/** \=============================================== MutualRecursion
  */
object MutualRecursion {
  def even(n: Int): Eval[Boolean] =
    Eval.always(n == 0).flatMap {
      case true  => Eval.True
      case false => odd(n - 1)
    }

  def odd(n: Int): Eval[Boolean] =
    Eval.always(n == 0).flatMap {
      case true  => Eval.False
      case false => even(n - 1)
    }
}

MutualRecursion.odd(199999).value
// res5: Boolean = true

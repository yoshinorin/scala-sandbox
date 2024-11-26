// https://typelevel.org/cats/typeclasses/functor.html
//> using dep "org.typelevel::cats-core:2.12.0"

import cats.Functor
import cats.syntax.all.*
import cats.data.Nested

val listOption = List(Some(1), None, Some(2))
// listOption: List[Option[Int]] = List(Some(1), None, Some(2))
println(listOption)

// Through Functor#compose
val composed = Functor[List].compose[Option].map(listOption)(_ + 1)
// res1: List[Option[Int]] = List(Some(value = 2), None, Some(value = 3))
println(composed)

def needsFunctor[F[_]: Functor, A](fa: F[A]): F[Unit] =
  Functor[F].map(fa)(_ => ())

def foo: List[Option[Unit]] = {
  val listOptionFunctor = Functor[List].compose[Option]
  type ListOption[A] = List[Option[A]]
  needsFunctor[ListOption, Int](listOption)(listOptionFunctor)
}

val nested: Nested[List, Option, Int] = Nested(listOption)
// nested: Nested[List, Option, Int] = Nested(
//   value = List(Some(value = 1), None, Some(value = 2))
// )
println(nested)
println(nested.map(_ + 1))
// res2: Nested[List, Option, Int] = Nested(
//   value = List(Some(value = 2), None, Some(value = 3))
// )

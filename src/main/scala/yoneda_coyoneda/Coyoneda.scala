package yoneda_coyoneda

import cats.Functor

sealed trait Coyoneda[F[_], A] {
  type UnderlyingType

  val underlyingValue: F[UnderlyingType]

  val transformation: UnderlyingType => A

  def run(f: Functor[F]) = f.map(underlyingValue)(transformation)

  def map[B](f: A => B): Coyoneda[F, B] =
    Coyoneda.toCoyoneda(underlyingValue)(transformation andThen f)
}

object Coyoneda {
  def toCoyoneda[F[_], A, B](fa: F[A])(f: A => B) : Coyoneda[F, B]  =
    new Coyoneda[F, B] {
      type UnderlyingType = A
      val transformation = f
      val underlyingValue = fa
  }
}

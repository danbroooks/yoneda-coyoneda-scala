package yoneda_coyoneda

import cats.Functor

abstract class Yoneda[F[_], A] { self =>
  def transformation[B](f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B): Yoneda[F, B] = new Yoneda[F, B] {
    def transformation[C](g: (B) => C): F[C] = self.transformation(g compose f)
  }
}

object Yoneda {
  def toYoneda[F[_], A](fa: F[A])(implicit F: Functor[F]): Yoneda[F, A] =
    new Yoneda[F, A] {
      override def transformation[B](f: (A) => B): F[B] = F.map(fa)(f)
    }

  def fromYoneda[F[_], A](lf: Yoneda[F, A]): F[A] = lf.run
}

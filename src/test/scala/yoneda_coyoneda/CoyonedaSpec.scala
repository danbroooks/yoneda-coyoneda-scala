package yoneda_coyoneda

import cats.Functor
import org.scalatest._

class CoyonedaSpec extends FreeSpec with Matchers {
  "Coyoneda" - {
    case class Person[A](age: A)

    "toCoyoneda" in {
      val personCoyo: Coyoneda[Person, Int] =
        Coyoneda.toCoyoneda(Person(42))(identity).map(_ + 1).map(_ + 2).map(_ + 3)

      val personFunctor = new Functor[Person] {
        override def map[A, B](fa: Person[A])(f: (A) => B): Person[B] = Person(f(fa.age))
      }

      val result = personCoyo.run(personFunctor)
      result should be(Person(48))
    }
  }
}

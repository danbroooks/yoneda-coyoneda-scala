package yoneda_coyoneda

import cats.implicits._
import org.scalatest._

class YonedaSpec extends FreeSpec with Matchers {
  "Yoneda" - {
    "toYoneda Option" in {
      var result = Yoneda.toYoneda(Option(40)).map(_ + 10).map(_ * 2).map(_ + 3).run
      result should be(Some(103))
    }

    "toYoneda List" in {
      var result = Yoneda.toYoneda(List(1, 2, 3)).map(_ * 2).map(_ + 10).run
      result should be(List(12, 14, 16))
    }
  }
}

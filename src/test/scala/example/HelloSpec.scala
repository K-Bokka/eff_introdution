package example

import org.scalatest._
import flatspec._
import matchers._

class HelloSpec extends AnyFlatSpec with should.Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }
}

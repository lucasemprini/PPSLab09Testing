package lecture09

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class BasicTest extends FunSuite with Matchers {
  test("Simple test"){
    true shouldBe true
  }
}

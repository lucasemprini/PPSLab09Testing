package lecture09

import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.{exists, forAll}
import org.scalatest.junit.JUnitRunner

// https://github.com/oscarrenalias/scalacheck-cookbook/blob/master/markdown/scalacheck-integration.md
@RunWith(classOf[ScalaCheckJUnitRunner])
class TestOnNumbers extends Properties("Numbers") {
  property("Sum is associative") = forAll{ (a: Int, b: Int, c: Int) =>
    (a+b)+c == a+(b+c)
  }
  property("Sum is commutative") = forAll { (a: Int, b: Int) =>
    a+b == b+a
  }
  property("Sum has zero as identity") = forAll { (a: Int) =>
    a + 0 == a && 0 + a == a
  }
  property("Diff is not associative") = forAll{ (a: Int, b: Int) =>
    exists{ c:Int => (a-b)-c != a-(b-c) }
  }
}

@RunWith(classOf[ScalaCheckJUnitRunner])
class TestOnLists extends Properties("Seqs") {
  def genericSizeProp[A:Arbitrary]: Prop = forAll{ (l1: Seq[A], l2: Seq[A]) =>
    (l1++l2).size == l1.size + l2.size
  }
  property("Size of concatenation") = Prop.all(
    genericSizeProp[Int], genericSizeProp[String], genericSizeProp[(Boolean,Double)])
  property("Reverse") = forAll { (l1: Seq[Int], l2: Seq[Int]) =>
    l1.reverse.reverse == l1
  }

  def associativeProp[A: Arbitrary]: Prop = forAll{(l1: Seq[A], l2: Seq[A], l3: Seq[A]) =>
    ( l1 ++ l2 ) ++ l3 == l1 ++ ( l2 ++ l3 )
  }

  property("Associative lists") = Prop.all(
    associativeProp[Int], associativeProp[String], associativeProp[(Boolean,Double)])

  def addToEmptyProp[A: Arbitrary]: Prop = forAll{(l1: Seq[A]) =>
     Seq.empty ++ l1 == l1
  }

  property("Add to empty list") = Prop.all(
    addToEmptyProp[Int], addToEmptyProp[String], addToEmptyProp[(Boolean,Double)])


  def mapIdentityProp[A: Arbitrary]: Prop = forAll{(l1: Seq[A]) =>
    l1.map(identity) == l1
  }

  property("Map identity list") = Prop.all(
    mapIdentityProp[Int], mapIdentityProp[String], mapIdentityProp[(Boolean,Double)])

  def mapComposeProp[A: Arbitrary, B: Arbitrary, C: Arbitrary]: Prop = forAll{(f: A=> B, g: B=> C, l1: Seq[A]) =>
    l1.map(g compose f) == l1.map(f).map(g)
  }

  property("Map compose list") = Prop.all(
    mapComposeProp[Int, Int, Int], mapComposeProp[String, String, String], mapComposeProp[(Boolean,Double), (Boolean,Double), (Boolean,Double)])

}
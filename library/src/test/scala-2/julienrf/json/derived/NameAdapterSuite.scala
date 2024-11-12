package julienrf.json.derived

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.Checkers
import play.api.libs.json._

class NameAdapterSuite extends AnyFeatureSpec with Checkers {

  Feature("use camelCase as the default casing for field names") {

    Scenario("product type") {
      case class Foo(sC: String, iC: Int)
      implicit val fooArbitrary: Arbitrary[(Foo, JsValue)] =
        Arbitrary(for (s <- Gen.alphaStr; i <- arbitrary[Int]) yield (Foo(s, i), Json.obj("sC" -> s, "iC" -> i)))
      implicit val fooFormat: OFormat[Foo] = oformat()
      jsonIdentityLaw[Foo]
    }

  }

  Feature("customize the casing for field names") {

    Scenario("product type") {
      case class Foo(sC: String, iC: Int)
      implicit val fooArbitrary: Arbitrary[(Foo, JsValue)] =
        Arbitrary(for (s <- Gen.alphaStr; i <- arbitrary[Int]) yield (Foo(s, i), Json.obj("s_c" -> s, "i_c" -> i)))
      implicit val fooFormat: OFormat[Foo] = oformat(snakeAdapter())
      jsonIdentityLaw[Foo]
    }

    Scenario("sum types") {
      sealed trait Foo
      case class Bar(xC: Int) extends Foo
      case class Baz(sC: String) extends Foo
      case object Bah extends Foo
      implicit lazy val fooFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String], snakeAdapter())

      implicit val fooArbitrary: Arbitrary[(Foo, JsValue)] =
        Arbitrary(
          Gen.oneOf(
            arbitrary[Int].map(i => (Bar(i), Json.obj("x_c" -> i, "type" -> "Bar"))),
            Gen.alphaStr.map(s => (Baz(s), Json.obj("s_c" -> s, "type" -> "Baz"))),
            Gen.const((Bah, Json.obj("type" -> "Bah"))
            )
          ))


      jsonIdentityLaw[Foo]
    }

    Scenario("sum types with options") {
      sealed trait Foo
      case class Bar(xC: Int) extends Foo
      case class Baz(sC: String) extends Foo
      case object Bah extends Foo
      case class Bat(oC: Option[String]) extends Foo

      implicit val fooFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String], snakeAdapter())

      implicit val fooArbitrary: Arbitrary[(Foo, JsValue)] =
        Arbitrary(
          Gen.oneOf(
            arbitrary[Int].map(i => (Bar(i), Json.obj("x_c" -> i, "type" -> "Bar"))),
            Gen.alphaStr.map(s => (Baz(s), Json.obj("s_c" -> s, "type" -> "Baz"))),
            Gen.const((Bah, Json.obj("type" -> "Bah"))),
            arbitrary[Option[String]].map(s => (Bat(s), Json.obj("type" -> "Bat") ++ s.fold(Json.obj())(x => Json.obj("o_c" -> x))))
          ))

      jsonIdentityLaw[Foo]
    }



    Scenario("recursive type") {
      sealed trait Tree
      case class Leaf(lS: String) extends Tree
      case class Node(lhsSnake: Tree, rhsSnake: Tree) extends Tree

      def writeTree(tree: Tree): JsValue = tree match {
        case n: Node =>
          Json.obj(
            "type" -> "Node",
            "lhs_snake" -> writeTree(n.lhsSnake),
            "rhs_snake" -> writeTree(n.rhsSnake)
          )
        case l: Leaf => Json.obj(
          "type" -> "Leaf",
          "l_s" -> l.lS
        )
      }

      implicit val arbitraryTree: Arbitrary[Tree] = {
        def atDepth(depth: Int): Gen[Tree] =
          if (depth < 3) {
            Gen.oneOf(
              arbitrary[String].map(Leaf),
              for {
                lhs <- atDepth(depth + 1)
                rhs <- atDepth(depth + 1)
              } yield Node(lhs, rhs)
            )
          } else arbitrary[String].map(Leaf)
        Arbitrary(atDepth(0))
      }

      implicit val arbitraryTreeWithJsValue: Arbitrary[(Tree, JsValue)] = {
        Arbitrary(for (t <- arbitrary[Tree]) yield (t, writeTree(t)))
      }

      {
        lazy val treeReads: Reads[Tree] = flat.reads[Tree]((__ \ "type").read[String], snakeAdapter(1))
        lazy val treeWrites: OWrites[Tree] = flat.owrites((__ \ "type").write[String], snakeAdapter(1))
        implicit lazy val treeFormat: OFormat[Tree] = OFormat.apply[Tree](treeReads, treeWrites)
        jsonIdentityLaw[Tree]
      }
    }

  }

  def snakeAdapter(max:Int = 2) = new NameAdapter {
    var nameMap = Map[String, Int]()

    def increment(v1: String) = this.synchronized {
      nameMap.get(v1).fold(nameMap += v1 -> 1)(i => nameMap += v1 -> (i + 1))
      if (nameMap(v1) > max) throw new RuntimeException(s"Snake conversion applied more than $max times to field: $v1")
    }

    override def apply(v1: String): String = {
      increment(v1)
      NameAdapter.snakeCase(v1)
    }
  }


  def jsonIdentityLaw[A](implicit reads: Reads[A], owrites: OWrites[A], arbA: Arbitrary[(A, JsValue)]): Unit =
    check((a: (A, JsValue)) => {
      reads.reads(a._2).fold(_ => false, r => r == a._1 && owrites.writes(r) == a._2)
    })

}

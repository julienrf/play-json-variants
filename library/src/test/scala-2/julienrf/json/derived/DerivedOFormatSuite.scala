package julienrf.json.derived

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatestplus.scalacheck.Checkers
import play.api.libs.json.{Format, JsArray, JsNumber, JsObject, Json, OFormat, OWrites, Reads, Writes, __}

class DerivedOFormatSuite extends AnyFeatureSpec with Checkers {

  Feature("encoding andThen decoding = identity") {

    Scenario("product type") {
      case class Foo(s: String, i: Int)
      implicit val fooArbitrary: Arbitrary[Foo] =
        Arbitrary(for (s <- arbitrary[String]; i <- arbitrary[Int]) yield Foo(s, i))
      implicit val fooFormat: OFormat[Foo] = oformat()
      identityLaw[Foo]
    }

    Scenario("tuple type") {
      type Foo = (String, Int)
      implicit val fooFormat: OFormat[Foo] = oformat()

      identityLaw[Foo]
    }

    Scenario("sum types") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo
      case object Bah extends Foo

      implicit val fooArbitrary: Arbitrary[Foo] =
        Arbitrary(
          Gen.oneOf(
            arbitrary[Int].map(Bar),
            arbitrary[String].map(Baz),
            Gen.const(Bah)
          )
        )

      {
        implicit val fooFormat: OFormat[Foo] = oformat()
        identityLaw[Foo]
      }
      {
        implicit val fooFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String])
        identityLaw[Foo]
      }
      {
        implicit val fooFormat: OFormat[Foo] = withTypeTag.oformat(TypeTagSetting.FullClassName)
        identityLaw[Foo]
      }
    }

    Scenario("recursive types") {
      sealed trait Tree
      case class Leaf(s: String) extends Tree
      case class Node(lhs: Tree, rhs: Tree) extends Tree

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

      {
        implicit lazy val treeFormat: OFormat[Tree] = oformat()
        identityLaw[Tree]
      }
      {
        implicit lazy val treeFormat: OFormat[Tree] = flat.oformat((__ \ "$type").format[String])
        identityLaw[Tree]
      }
    }

    Scenario("polylmorphic types") {
      case class Quux[A](value: A)
      implicit val fooFormat: OFormat[Quux[Int]] = oformat()
      implicit val arbitraryFoo: Arbitrary[Quux[Int]] =
        Arbitrary(arbitrary[Int].map(new Quux(_)))
      identityLaw[Quux[Int]]
    }
  }

  def identityLaw[A](implicit reads: Reads[A], owrites: OWrites[A], arbA: Arbitrary[A]): Unit =
    check((a: A) => reads.reads(owrites.writes(a)).fold(_ => false, _ == a))

  Feature("default codecs represent sum types using nested JSON objects") {
    Scenario("default codecs represent sum types using nested JSON objects") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo
      val fooFormat: OFormat[Foo] = oformat()
      assert(fooFormat.writes(Bar(42)) == Json.obj("Bar" -> Json.obj("x" -> JsNumber(42))))
    }
  }

  Feature("sum types JSON representation can be customized") {
    Scenario("sum types JSON representation can be customized") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo
      val fooFlatFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String])
      assert(fooFlatFormat.writes(Bar(42)) == Json.obj("type" -> "Bar", "x" -> JsNumber(42)))
    }
  }

  Feature("case classes can have optional values") {
    case class Foo(s: Option[String])
    implicit val fooFormat: OFormat[Foo] = oformat()
    implicit val arbitraryFoo: Arbitrary[Foo] =
      Arbitrary(for (s <- arbitrary[Option[String]]) yield Foo(s))

    Scenario("identity law") {
      identityLaw[Foo]
    }

    Scenario("Missing fields are successfully decoded as `None`") {
      assert(fooFormat.reads(Json.obj()).asOpt.contains(Foo(None)))
    }

    Scenario("Wrong fields are errors") {
      assert(fooFormat.reads(Json.obj("s" -> 42)).asOpt.isEmpty)
    }

    Scenario("Nested objects") {
      case class Bar(foo: Foo)
      implicit val barFormat: OFormat[Bar] = oformat()
      implicit val arbitraryBar: Arbitrary[Bar] =
        Arbitrary(for (foo <- arbitrary[Foo]) yield Bar(foo))

      identityLaw[Bar]
      assert(barFormat.reads(Json.obj("foo" -> Json.obj())).asOpt.contains(Bar(Foo(None))))
//      See https://github.com/playframework/playframework/issues/5863
//      assert(barFormat.reads(Json.obj("foo" -> 42)).asOpt.isEmpty)
    }
  }

  Feature("error messages must be helpful") {
    Scenario("error messages must be helpful") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo
      val fooFlatFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String])
      val readResult = fooFlatFormat.reads(Json.parse("""{"type": "Bar", "x": "string"}"""))
      val errorString = readResult.fold(
        _.flatMap { case (path, errors) =>
          errors.map(_.message + (if (path != __) " at " + path.toString() else "")) }.sorted.mkString("; "),
        _ => "No Errors")
      assert(errorString == "error.expected.jsnumber at /x; error.sealed.trait")
    }
  }

  Feature("type tags") {
    import TestHelpers._
    import julienrf.json.derived

    Scenario("user-defined type tags") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo

      implicit val barTypeTag: CustomTypeTag[Bar] = CustomTypeTag("_bar_")
      implicit val bazTypeTag: CustomTypeTag[Baz] = CustomTypeTag("_baz_")

      implicit val fooFormat: OFormat[Foo] = withTypeTag.oformat(TypeTagSetting.UserDefinedName)

      val foo: Foo = Bar(42)
      val json = fooFormat.writes(Bar(42))
      assert(json == Json.obj("_bar_" -> Json.obj("x" -> 42)))
      assert(fooFormat.reads(json).asEither == Right(foo))
    }

    Scenario("ShortClassNameSnakeCase should format tag names using snake casing") {
      implicit lazy val demoClassFormat: OFormat[CompositeNameClass] = derived
        .withTypeTag
        .oformat[CompositeNameClass](TypeTagSetting.ShortClassNameSnakeCase)

      val inner = Seq(FooBar(true), fooBarry(true), foo_barrier(true))
      val barFoo: CompositeNameClass = BarFoo(inner)
      val parsed = Json.toJsObject(barFoo)
      val parsedInner = ((parsed \ "bar_foo") \ "inner")
        .as[JsArray]
        .value
        .map(_.as[JsObject])

      assert(validSnakeNames.contains(parsed.keys.head))
      assert(parsedInner.map(_.keys.head).forall(validSnakeNames.contains))
    }
  }

  Feature("user-defined implicits") {
    Scenario("user-defined implicits are not overridden by derived implicits - nested") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo

      object Bar {
        implicit val format: Format[Bar] = {
          val writes = Writes[Bar] { bar =>
            Json.obj("y" -> bar.x)
          }
          val reads = Reads { json =>
            (json \ "y").validate[Int].map(Bar.apply)
          }

          Format(reads, writes)
        }
      }

      implicit val fooFormat = oformat[Foo]()

      val foo: Foo = Bar(42)
      val json = fooFormat.writes(foo)

      assert(json == Json.obj("Bar" -> Json.obj("y" -> JsNumber(42))))
      assert(fooFormat.reads(json).asEither == Right(foo))
    }

    Scenario("user-defined implicits are not overridden by derived implicits - flat") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo

      object Bar {
        implicit val format: Format[Bar] = {
          val writes = Writes[Bar] { bar =>
            Json.obj("y" -> bar.x)
          }
          val reads = Reads { json =>
            (json \ "y").validate[Int].map(Bar.apply)
          }

          Format(reads, writes)
        }
      }

      implicit val fooFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String])

      val foo: Foo = Bar(42)
      val json = fooFormat.writes(foo)

      assert(json == Json.obj("type" -> "Bar", "y" -> JsNumber(42)))
      assert(fooFormat.reads(json).asEither == Right(foo))
    }

    Scenario("supports user-defined value-formats") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo

      object Bar {
        implicit val format: Format[Bar] = Json.valueFormat
      }

      implicit val fooFormat = oformat[Foo]()

      val foo: Foo = Bar(42)
      val json = fooFormat.writes(foo)

      assert(json == Json.obj("Bar" -> JsNumber(42)))
      assert(fooFormat.reads(json).asEither == Right(foo))
    }

    Scenario("supports user-defined value-formats for the flat format by synthesizing a wrapper") {
      sealed trait Foo
      case class Bar(x: Int) extends Foo
      case class Baz(s: String) extends Foo

      object Bar {
        implicit val format: Format[Bar] = Json.valueFormat
      }

      implicit val fooFormat: OFormat[Foo] = flat.oformat((__ \ "type").format[String])

      val foo: Foo = Bar(42)
      val json = fooFormat.writes(foo)

      assert(json == Json.obj("type" -> "Bar", "__syntheticWrap__" -> JsNumber(42)))
      assert(fooFormat.reads(json).asEither == Right(foo))
    }

    import TestHelpers._
    val adt = Z(X(1), Y("VVV"))

    Scenario("supports user-defined recursive formats - nested") {
      val adtFormat: Format[ADTBase] = {
        implicit val f1: Format[X] = Json.format
        implicit val f2: Format[Y] = Json.format
        implicit lazy val f3: Format[Z] = Json.format

        implicit lazy val f4: Format[ADTBase] = oformat[ADTBase]()

        f4
      }

      val json = adtFormat.writes(adt)
      val obj = Json.obj(
        "Z" -> Json.obj(
          "l" -> Json.obj("X" -> Json.obj("a" -> 1)),
          "r" -> Json.obj("Y" -> Json.obj("b" -> "VVV"))))

      assert(json == obj)
      assert(adtFormat.reads(json).asEither == Right(adt))
    }

    Scenario("supports user-defined recursive formats - flat") {
      val adtFormat: Format[ADTBase] = {
        implicit val f1: Format[X] = Json.format
        implicit val f2: Format[Y] = Json.format
        implicit lazy val f3: Format[Z] = Json.format

        implicit lazy val f4: Format[ADTBase] = flat.oformat((__ \ "type").format[String])

        f4
      }

      val json = adtFormat.writes(adt)
      val obj = Json.obj(
        "type" -> "Z",
        "l" -> Json.obj("type" -> "X", "a" -> 1),
        "r" -> Json.obj("type" -> "Y", "b" -> "VVV"))

      assert(json == obj)
      assert(adtFormat.reads(json).asEither == Right(adt))
    }
  }
}


object TestHelpers {
  // Placing it here in a separate object since otherwise the Json.format macro fails to compile
  // for these types
  sealed trait ADTBase

  case class X(a: Int) extends ADTBase
  case class Y(b: String) extends ADTBase
  case class Z(l: ADTBase, r: ADTBase) extends ADTBase

  sealed trait CompositeNameClass

  case class FooBar(inner: Boolean) extends CompositeNameClass
  case class fooBarry(inner: Boolean) extends CompositeNameClass
  case class foo_barrier(inner: Boolean) extends CompositeNameClass
  case class BarFoo(inner: Seq[CompositeNameClass]) extends CompositeNameClass

  lazy val validSnakeNames: Set[String] =
    Set("foo_bar", "foo_barry", "foo_barrier", "bar_foo")
}

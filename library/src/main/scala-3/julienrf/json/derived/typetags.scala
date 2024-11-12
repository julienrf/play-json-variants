package julienrf.json.derived

import play.api.libs.json.*

import scala.compiletime.summonInline
import scala.quoted.{Expr, Quotes, Type}
import scala.reflect.ClassTag

trait TypeTagOWrites {
  def owrites[A](typeName: String, writes: Writes[A]): OWrites[A]
}

object TypeTagOWrites {
  val nested: TypeTagOWrites = new TypeTagOWrites {
    def owrites[A](typeName: String, writes: Writes[A]): OWrites[A] =
      OWrites[A](a => Json.obj(typeName -> writes.writes(a)))
  }

  def flat(tagOwrites: OWrites[String]): TypeTagOWrites = new TypeTagOWrites {
    def owrites[A](typeName: String, writes: Writes[A]): OWrites[A] =
      OWrites[A] { a =>
        val origJson = writes.writes(a)
        val wrappedObj = origJson match {
          case obj: JsObject =>
            obj
          case nonObj => SyntheticWrapper.write(nonObj)
        }
        tagOwrites.writes(typeName) ++ wrappedObj
      }
  }
}

trait TypeTagReads {
  def reads[A](typeName: String, reads: Reads[A]): Reads[A]
}

object TypeTagReads {
  val nested: TypeTagReads = new TypeTagReads {
    def reads[A](typeName: String, reads: Reads[A]): Reads[A] =
      (__ \ typeName).read(reads)
  }

  def flat(tagReads: Reads[String]): TypeTagReads = new TypeTagReads {
    def reads[A](typeName: String, reads: Reads[A]): Reads[A] = {
      val withSyntheticFallback = SyntheticWrapper.reads(reads)
      tagReads.filter(_ == typeName).flatMap(_ => withSyntheticFallback)
    }
  }
}

private[derived] object SyntheticWrapper {
  private val syntheticField = "__syntheticWrap__"

  def isSynthetic(obj: JsObject): Boolean =
    obj.keys.contains(syntheticField)

  def write(inner: JsValue): JsObject = Json.obj(syntheticField -> inner)

  def reads[A](inner: Reads[A]): Reads[A] =
    Reads {
      case obj: JsObject if isSynthetic(obj) =>
        (obj \ syntheticField).validate(inner)
      case nonSynthetic => inner.reads(nonSynthetic)
    }
}

trait TypeTagOFormat extends TypeTagReads with TypeTagOWrites

object TypeTagOFormat {
  def apply(ttReads: TypeTagReads, ttOWrites: TypeTagOWrites): TypeTagOFormat =
    new TypeTagOFormat {
      def reads[A](typeName: String, reads: Reads[A]): Reads[A] = ttReads.reads(typeName, reads)

      def owrites[A](typeName: String, writes: Writes[A]): OWrites[A] = ttOWrites.owrites(typeName, writes)
    }

  val nested: TypeTagOFormat = TypeTagOFormat(TypeTagReads.nested, TypeTagOWrites.nested)

  def flat(tagFormat: OFormat[String]): TypeTagOFormat =
    TypeTagOFormat(TypeTagReads.flat(tagFormat), TypeTagOWrites.flat(tagFormat))
}

trait TypeTag[A] {
  def value: String
}

object TypeTag {
  trait ShortClassName[A] extends TypeTag[A]

  object ShortClassName {
    given fromClassTag[A](using ct: ClassTag[A]): ShortClassName[A] with {
      def value: String = ct.runtimeClass.getSimpleName.stripSuffix("$")
    }
  }


  trait ShortClassNameSnakeCase[A] extends TypeTag[A]

  object ShortClassNameSnakeCase {
    given fromClassTag[A](using ct: ClassTag[A]): ShortClassNameSnakeCase[A] with {
      def value: String = play.api.libs.json.JsonNaming.SnakeCase.apply(ct.runtimeClass.getSimpleName)
    }
  }

  trait FullClassName[A] extends TypeTag[A]

  object FullClassName {
    given fromClassTag[A](using ct: ClassTag[A]): FullClassName[A] with {
      def value: String = ct.runtimeClass.getName
    }
  }

  given fromClassTagToFullClassName[A]: Conversion[ClassTag[A], FullClassName[A]] with
    override def apply(using ct: ClassTag[A]): FullClassName[A] = new FullClassName[A] {
      def value: String = ct.runtimeClass.getName
    }

  trait UserDefinedName[A] extends TypeTag[A]

  object UserDefinedName {
    given fromCustomTypeTag[A](using ctt: CustomTypeTag[A]): UserDefinedName[A] with {
      def value: String = ctt.typeTag
    }
  }
  //  given fromCustomTypeTag[A]: Conversion[CustomTypeTag[A], UserDefinedName[A]] with
  //    override def apply(using ctt: CustomTypeTag[A]): UserDefinedName[A] = new UserDefinedName[A] {
  //      def value: String = ctt.typeTag
  //    }

  //  inline given fromCustomTypeTag[A]: UserDefinedName[A] = inline summonInline[CustomTypeTag[A]] match {
  //    case ctt: CustomTypeTag[A] => new UserDefinedName[A] {
  //      def value: String = ctt.typeTag
  //    }
  //  }


}

case class CustomTypeTag[A](typeTag: String)

trait TypeTagSetting {
  type Value[A] <: TypeTag[A]
}

object TypeTagSetting {
  object ShortClassName extends TypeTagSetting {
    type Value[A] = TypeTag.ShortClassName[A]
  }

  object ShortClassNameSnakeCase extends TypeTagSetting {
    type Value[A] = TypeTag.ShortClassNameSnakeCase[A]
  }

  object FullClassName extends TypeTagSetting {
    type Value[A] = TypeTag.FullClassName[A]
  }

  object UserDefinedName extends TypeTagSetting {
    type Value[A] = TypeTag.UserDefinedName[A]
  }
}
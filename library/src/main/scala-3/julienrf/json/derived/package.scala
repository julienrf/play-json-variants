package julienrf.json

import play.api.libs.json.{OFormat, OWrites, Reads}

import scala.compiletime.summonFrom

package object derived {

  inline def reads[A](adapter: NameAdapter = NameAdapter.identity)(using derivedReads: DerivedReads[A, TypeTag.ShortClassName[A]]): Reads[A] =
    derivedReads.reads(TypeTagReads.nested, adapter)

  inline def owrites[A](adapter: NameAdapter = NameAdapter.identity)(using derivedOWrites: DerivedOWrites[A, TypeTag.ShortClassName[A]]): OWrites[A] =
    derivedOWrites.owrites(TypeTagOWrites.nested, adapter)

  inline def oformat[A](adapter: NameAdapter = NameAdapter.identity)(using derivedReads: DerivedReads[A, TypeTag.ShortClassName[A]], derivedOWrites: DerivedOWrites[A, TypeTag.ShortClassName[A]]): OFormat[A] =
    OFormat(derivedReads.reads(TypeTagReads.nested, adapter), derivedOWrites.owrites(TypeTagOWrites.nested, adapter))

  object flat {

    inline def reads[A](typeName: Reads[String], adapter: NameAdapter = NameAdapter.identity)(using derivedReads: DerivedReads[A, TypeTag.ShortClassName[A]]): Reads[A] =
      derivedReads.reads(TypeTagReads.flat(typeName), adapter)

    inline def owrites[A](typeName: OWrites[String], adapter: NameAdapter = NameAdapter.identity)(using derivedOWrites: DerivedOWrites[A, TypeTag.ShortClassName[A]]): OWrites[A] =
      derivedOWrites.owrites(TypeTagOWrites.flat(typeName), adapter)

    inline def oformat[A](typeName: OFormat[String], adapter: NameAdapter = NameAdapter.identity)(using derivedReads: DerivedReads[A, TypeTag.ShortClassName[A]], derivedOWrites: DerivedOWrites[A, TypeTag.ShortClassName[A]]): OFormat[A] =
      OFormat(derivedReads.reads(TypeTagReads.flat(typeName), adapter), derivedOWrites.owrites(TypeTagOWrites.flat(typeName), adapter))

  }

  object withTypeTag {

    inline def reads[A](typeTagSetting: TypeTagSetting, adapter: NameAdapter = NameAdapter.identity, typeTagReads: TypeTagReads = TypeTagReads.nested)(using derivedReads: DerivedReads[A, typeTagSetting.Value[A]]): Reads[A] =
      derivedReads.reads(typeTagReads, adapter)

    inline def owrites[A](typeTagSetting: TypeTagSetting, adapter: NameAdapter = NameAdapter.identity, typeTagOWrites: TypeTagOWrites = TypeTagOWrites.nested)(using derivedOWrites: DerivedOWrites[A, typeTagSetting.Value[A]]): OWrites[A] =
      derivedOWrites.owrites(typeTagOWrites, adapter)

    inline def oformat[A](typeTagSetting: TypeTagSetting, adapter: NameAdapter = NameAdapter.identity, typeTagOFormat: TypeTagOFormat = TypeTagOFormat.nested)(using derivedReads: DerivedReads[A, typeTagSetting.Value[A]], derivedOWrites: DerivedOWrites[A, typeTagSetting.Value[A]]): OFormat[A] =
      OFormat(derivedReads.reads(typeTagOFormat, adapter), derivedOWrites.owrites(typeTagOFormat, adapter))

  }

  private[derived] inline def summonTypeTagOrFallback[A, TT[P] <: TypeTag[P]](fallback: => TypeTag[A]): TypeTag[A] =
    summonFrom[TT[A]] {
      case r: TT[A] => r
      case _ => fallback
    }

}
package julienrf.json.derived

import play.api.libs.json.{JsValue, JsObject, Json, OWrites, Writes}
import shapeless.labelled.FieldType
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

/**
  * Derives an `OWrites[A]`
  *
  * @tparam TT Type of TypeTag to use to discriminate alternatives of sealed traits
  */
trait DerivedOWrites[A, TT[A] <: TypeTag[A]] {

  /**
    * @param tagOwrites The strategy to use to serialize sum types
    * @param adapter The fields naming strategy
    * @return The derived `OWrites[A]`
    */
  def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[A]
}

object DerivedOWrites extends DerivedOWritesInstances

trait DerivedOWritesInstances extends DerivedOWritesInstances1 {
  /** Supports reading a coproduct where the left branch already has a defined `Writes` instance.
    * This will avoid using the `Generic` implicit for cases where it is possible to use a pre-existing
    * `Writes`, thus enabling users to easily plug-in their own `Format` as well as saving on compile-times.
    */
  implicit def owritesPredefinedCoProduct[K <: Symbol, L, R <: Coproduct, TT[A] <: TypeTag[A]](implicit
    writesL: Lazy[Writes[L]],
    owritesR: Lazy[DerivedOWrites[R, TT]],
    typeTag: TT[FieldType[K, L]]
  ): DerivedOWrites[FieldType[K, L] :+: R, TT] =
    DerivedOWritesUtil.makeCoProductOWrites(
      (_, _) => writesL,
      owritesR,
      typeTag)
}

trait DerivedOWritesInstances1 extends DerivedOWritesInstances2 {

  implicit def owritesHNil[TT[A] <: TypeTag[A]]: DerivedOWrites[HNil, TT] =
    new DerivedOWrites[HNil, TT] {
      def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter) = OWrites[HNil] { _ => Json.obj() }
    }

  implicit def owritesLabelledHListOpt[A, K <: Symbol, H, T <: HList, TT[A] <: TypeTag[A]](implicit
    fieldName: Witness.Aux[K],
    owritesH: Lazy[Writes[H]],
    owritesT: Lazy[DerivedOWrites[T, TT]]
  ): DerivedOWrites[FieldType[K, Option[H]] :: T, TT] =
    new DerivedOWrites[FieldType[K, Option[H]] :: T, TT] {
      def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter) = {
        val adaptedName = adapter(fieldName.value.name)
        val derivedOwriteT = owritesT.value.owrites(tagOwrites, adapter)
        OWrites[FieldType[K, Option[H]] :: T] { case maybeH :: t =>
          val maybeField: Map[String, JsValue] =
            (maybeH: Option[H]) match {
              case Some(h) => Map(adaptedName -> owritesH.value.writes(h))
              case None => Map.empty
            }
          JsObject(maybeField ++ derivedOwriteT.writes(t).value)
        }
      }
    }

  implicit def owritesCNil[TT[A] <: TypeTag[A]]: DerivedOWrites[CNil, TT] =
    new DerivedOWrites[CNil, TT] {
      def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[CNil] = OWrites {
        _ => sys.error("No JSON representation of CNil")
      }
    }

  implicit def owritesCoproduct[K <: Symbol, L, R <: Coproduct, TT[A] <: TypeTag[A]](implicit
    owritesL: Lazy[DerivedOWrites[L, TT]],
    owritesR: Lazy[DerivedOWrites[R, TT]],
    typeTag: TT[FieldType[K, L]]
  ): DerivedOWrites[FieldType[K, L] :+: R, TT] =
    DerivedOWritesUtil.makeCoProductOWrites(
      (tagOwrites, adapter) => owritesL.map(_.owrites(tagOwrites, adapter)),
      owritesR,
      typeTag)
}

trait DerivedOWritesInstances2 extends DerivedOWritesInstances3 {

  implicit def owritesLabelledHList[A, K <: Symbol, H, T <: HList, TT[A] <: TypeTag[A]](implicit
    fieldName: Witness.Aux[K],
    owritesH: Lazy[Writes[H]],
    owritesT: Lazy[DerivedOWrites[T, TT]]
  ): DerivedOWrites[FieldType[K, H] :: T, TT] =
    new DerivedOWrites[FieldType[K, H] :: T, TT] {
      def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter) = {
        val adaptedName = adapter(fieldName.value.name)
        val derivedOwritesT = owritesT.value.owrites(tagOwrites, adapter)
        OWrites[FieldType[K, H] :: T] { case h :: t =>
          JsObject(Map(adaptedName -> owritesH.value.writes(h)) ++ derivedOwritesT.writes(t).value)
        }
      }
    }

}

trait DerivedOWritesInstances3 {

  implicit def owritesGeneric[A, R, TT[A] <: TypeTag[A]](implicit
    gen: LabelledGeneric.Aux[A, R],
    derivedOWrites: Lazy[DerivedOWrites[R, TT]]
  ): DerivedOWrites[A, TT] =
    new DerivedOWrites[A, TT] {
      def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter) =
        OWrites.contravariantfunctorOWrites.contramap(derivedOWrites.value.owrites(tagOwrites, adapter), gen.to)
    }

}

private[derived] object DerivedOWritesUtil {
  def makeCoProductOWrites[K <: Symbol, L, R <: Coproduct, TT[A] <: TypeTag[A]](
    makeWritesL: (TypeTagOWrites, NameAdapter) => Lazy[Writes[L]],
    owritesR: Lazy[DerivedOWrites[R, TT]],
    typeTag: TT[FieldType[K, L]]
  ): DerivedOWrites[FieldType[K, L] :+: R, TT] =
    new DerivedOWrites[FieldType[K, L] :+: R, TT] {
      def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter) = {
        // we don't want to create the OWrites instance more than once
        val writesL = makeWritesL(tagOwrites, adapter)
        val derivedOwriteR = owritesR.value.owrites(tagOwrites, adapter)

        OWrites[FieldType[K, L] :+: R] {
          case Inl(l) => tagOwrites.owrites(typeTag.value, writesL.value).writes(l)
          case Inr(r) => derivedOwriteR.writes(r)
        }
      }
    }
}

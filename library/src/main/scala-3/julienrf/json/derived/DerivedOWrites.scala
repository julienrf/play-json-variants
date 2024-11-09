package julienrf.json.derived

import play.api.libs.json.*

import scala.compiletime.*
import scala.deriving.Mirror

/**
 * Derives an `OWrites[A]`
 *
 * @tparam TT Type of TypeTag to use to discriminate alternatives of sealed traits
 */
trait DerivedOWrites[A, TT <: TypeTag[A]] {

  /**
   * @param tagOwrites The strategy to use to serialize sum types
   * @param adapter    The fields naming strategy
   * @return The derived `OWrites[A]`
   */
  def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[A]
}

object DerivedOWrites extends DerivedOWritesInstances

trait DerivedOWritesInstances {
  inline given derivedSumWrites[A, TT[S] <: TypeTag[S]](using m: Mirror.SumOf[A]): DerivedOWrites[A, TT[A]] = new DerivedOWrites[A, TT[A]] {
    def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[A] = sumWrites[A, TT](m, tagOwrites, adapter)
  }

  inline given derivedProductWrites[A, TT[P] <: TypeTag[P]](using m: Mirror.ProductOf[A], tt: TT[A]): DerivedOWrites[A, TT[A]] = new DerivedOWrites[A, TT[A]] {
    def owrites(tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[A] = productWrites(m, tagOwrites, adapter)
  }


  private inline def sumWrites[A, TT[P] <: TypeTag[P]](s: Mirror.SumOf[A], tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[A] = new OWrites[A] {
    def writes(a: A): JsObject = {
      val ordinal = s.ordinal(a)
      val elemWritesAndTypeTags = summonAllWritesAndTypeTags[s.MirroredElemTypes, TT](tagOwrites, adapter)
      val (write, fieldName) = elemWritesAndTypeTags(ordinal)
      tagOwrites.owrites(fieldName, write.asInstanceOf[Writes[A]]).writes(a)
    }
  }

  private inline def productWrites[A](m: Mirror.Of[A], tagOwrites: TypeTagOWrites, adapter: NameAdapter): OWrites[A] = new OWrites[A] {
    private val elemLabels = summonAllLabels[m.MirroredElemLabels].map(adapter)
    private val elemWrites = summonAllWrites[m.MirroredElemTypes]

    def writes(a: A): JsObject = {
      val elems = a.asInstanceOf[Product].productIterator.toList
      val fields = elemLabels.zip(elems).zip(elemWrites).map {
        case ((label, value), writes) =>
          label -> writes.asInstanceOf[Writes[Any]].writes(value)
      }
      JsObject(fields)
    }
  }

  private inline def summonWrites[T]: Writes[T] = summonInline[Writes[T]]


  private inline def summonAllWritesAndTypeTags[T <: Tuple, TT[P] <: TypeTag[P]](tagOWrites: TypeTagOWrites, adapter: NameAdapter): List[(Writes[_], String)] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (hh *: ts) =>
        val (headWrite, headTagType) = summonWritesOrFallback[hh, TT] {
          val derivedOWrites = summonInline[DerivedOWrites[hh, TT[hh]]]
          val fieldName = summonInline[TT[hh]].value
          val writes = derivedOWrites.owrites(tagOWrites, adapter)
          (writes, fieldName)
        }
        (headWrite, headTagType) :: summonAllWritesAndTypeTags[ts, TT](tagOWrites, adapter)


  private inline def summonAllWrites[T <: Tuple]: List[Writes[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (hh *: ts) => summonWrites[hh] :: summonAllWrites[ts]

  private inline def summonAllLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (hx *: tx) => constValue[hx].asInstanceOf[String] :: summonAllLabels[tx]

  private inline def summonWritesOrFallback[T, TT[P] <: TypeTag[P]](fallback: => (Writes[T], String)): (Writes[T], String) =
    summonFrom[Writes[T]] {
      case r: Writes[T] => r -> summonInline[TT[T]].value
      case _ => fallback
    }
}
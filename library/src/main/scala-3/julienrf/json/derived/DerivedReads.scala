package julienrf.json.derived

import play.api.libs.json.*

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}


trait DerivedReads[A, TT <: TypeTag[A]] {
  def reads(tagReads: TypeTagReads, adapter: NameAdapter): Reads[A]
}

object DerivedReads {
  inline given derivedSumReads[A, TT[S] <: TypeTag[S]](using m: Mirror.SumOf[A]): DerivedReads[A, TT[A]] = new DerivedReads[A, TT[A]] {
    def reads(tagReads: TypeTagReads, adapter: NameAdapter): Reads[A] = sumReads[A, TT](m, tagReads, adapter)
  }

  inline given derivedProductReads[A, TT <: TypeTag[A]](using m: Mirror.ProductOf[A], tt: TT): DerivedReads[A, TT] = new DerivedReads[A, TT] {
    def reads(tagReads: TypeTagReads, adapter: NameAdapter): Reads[A] = productReads(m, adapter)
  }

  private inline def summonReads[T]: Reads[T] = summonInline[Reads[T]]

  private inline def summonAllReads[T <: Tuple]: List[(Reads[_], Boolean)] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (Option[s] *: ts) =>
        (summonReads[s], true) :: summonAllReads[ts]
      case _: (s *: ts) =>
        (summonReads[s], false) :: summonAllReads[ts]

  private inline def summonAllTypes[T <: Tuple]: List[_] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (s *: ts) => erasedValue[s] :: summonAllTypes[ts]

  private inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (head *: tail) => constValue[head].asInstanceOf[String] :: summonLabels[tail]

  private inline def summonReadsOrFallback[T, TT[P] <: TypeTag[P]](fallback: => (Reads[T], String)): (Reads[T], String) =
    summonFrom[Reads[T]] {
      case r: Reads[T] => r -> summonInline[TT[T]].value
      case _ => fallback
    }

  private inline def productReads[A, TT <: TypeTag[A]](m: Mirror.ProductOf[A], adapter: NameAdapter)(using typeTag: TT): Reads[A] =
    val elemReads = summonAllReads[m.MirroredElemTypes]
    val elemLabels = summonLabels[m.MirroredElemLabels].map(adapter)
    
    Reads { json =>

      val results = elemLabels.zip(elemReads).map {
        case (label, (read, false)) =>
          (json \ label).validate(read.asInstanceOf[Reads[Any]])
        case (label, (read, true)) =>
          (json \ label).validateOpt(read.asInstanceOf[Reads[Any]])
      }

      results.collect { case JsError(errors) => errors } match
        case Nil =>
          val args = results.map(_.get)
          Try(m.fromProduct(Tuple.fromArray(args.toArray))) match
            case Success(value: A) => JsSuccess(value)
            case Failure(ex) => JsError(s"Error constructing instance: ${ex.getMessage}")
        case errors =>
          JsError(errors.flatten)
    }


  private inline def handleSumTypes[T <: Tuple, A, TT[P] <: TypeTag[P]](tagReads: TypeTagReads, json: JsValue, adapter: NameAdapter): JsResult[A] = inline erasedValue[T] match {
    case _: (h *: ts) =>
      val (reads, fieldName) = summonReadsOrFallback[h, TT] {
        val derivedReads = summonInline[DerivedReads[h, TT[h]]]
        val reads = derivedReads.reads(tagReads, adapter)
        reads -> summonInline[TT[h]].value
      }

      tagReads.reads(fieldName, reads).reads(json) match {
        case JsSuccess(value, _) =>
          JsSuccess(value.asInstanceOf[A])
        case JsError(_) => handleSumTypes[ts, A, TT](tagReads, json, adapter)
      }
    case _: EmptyTuple =>
      JsError(s"Could not find a matching type for $json")
  }


  inline def sumReads[A, TT[P] <: TypeTag[P]](s: Mirror.SumOf[A], tagReads: TypeTagReads, adapter: NameAdapter): Reads[A] = new Reads[A] {
    def reads(json: JsValue): JsResult[A] = handleSumTypes[s.MirroredElemTypes, A, TT](tagReads, json, adapter)
  }

}
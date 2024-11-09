package julienrf.json.derived

/** Adapter function to transform case classes member names during the derivation process
 *
 * A NameAdapter can be used to customize the derivation process, allowing to apply a transformation function
 * to case classes member names when deriving serializers/deserializers
 *
 * For instance, it can be used to derive serializers/deserializers that use a different casing for the json keys.
 *
 * For example, to derive a Format[A] that uses snake_case for the json keys (using the predefined [[NameAdapter.snakeCase]])
 * {{{
 * import julienrf.json.derived
 * import julienrf.json.derived.NameAdapter
 * import play.api.libs.json.{Format, Json}
 *
 * case class Bar(camelCase: String)
 * object Bar {
 *   implicit val format: Format[Bar] = derived.oformat[Bar](NameAdapter.snakeCase)
 * }
 * }}}
 *
 * {{{
 * scala> Json.toJson(Bar("a json value"))
 * res0: play.api.libs.json.JsValue = {"camel_case":"a json value"}
 *
 * scala> Json.fromJson[Bar](Json.parse("""{"camel_case":"a json value"}"""))
 * res1: play.api.libs.json.JsResult[Bar] = JsSuccess(Bar(a json value),)
 * }}}
 */
trait NameAdapter extends (String => String)

object NameAdapter {

  /** Converts case classes member names from camelCase to snake_case */
  val snakeCase = NameAdapter {
    (s: String) => {
      val builder = new StringBuilder
      s foreach {
        case c if Character.isUpperCase(c) && builder.nonEmpty =>
          builder append "_" append (Character toLowerCase c)
        case c => builder append c
      }
      builder.toString
    }
  }

  /** Does not apply any transformation to case classes member names */
  val identity = NameAdapter(s => s)

  def apply(f: (String => String)): NameAdapter = new NameAdapter {
    override def apply(v1: String): String = f(v1)
  }
}

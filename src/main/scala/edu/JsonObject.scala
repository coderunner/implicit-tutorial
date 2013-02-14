package edu

/**
 * JSON object representation.
 */
class JsonObject(properties: Map[JsonKey, JsonValueType]) {

  /**
   * Get the value associated with the property 'name'.
   *
   * @param name the name of the property
   * @return an optional value associated with the name.
   */
  def property(name: JsonKey): Option[Any] = properties.get(name)

  /**
   * Returns an iterator over all of the JSON object properties tuples.
   *
   * @return an iterator over all of the JSON object properties tuples.
   */
  def allProperties: Iterator[(JsonKey, JsonValueType)] = properties.iterator
}

object JsonObject {

  def apply(properties: Map[JsonKey, JsonValueType]) = new JsonObject(properties)
  implicit def mapToJsonObject(jsonMap: Map[JsonKey, JsonValueType]) = JsonObject(jsonMap)
  implicit def stringToJsonString(s: String) = StringValueType(s)
  implicit def intToJsonInt(i: Int) = IntValueType(i)

  implicit def stringToJsonKey(name: String) = new JsonKey(name)

  implicit val defaultRenderer = PrettyJsonRenderer
  def renderJson(json: JsonObject)(implicit renderer: JsonRenderer): String = {
    renderer.renderJson(json)
  }
}

trait JsonRenderer {
  def renderJson(json: JsonObject): String
}

object PrettyJsonRenderer extends JsonRenderer {
  val OPEN_BRACE = "{\n"
  val COLON = " : "
  val COMMA = ",\n"
  val QUOTES = "\""
  val CLOSING_BRACE = "\n}\n"

  def renderJson(json: JsonObject) = json.allProperties.map(renderProperty).mkString(OPEN_BRACE, COMMA, CLOSING_BRACE)

  private def renderProperty(prop: (JsonKey, JsonValueType)): String = {
    val (name, value) = prop
    name + COLON + renderValue(value)
  }

  private def renderValue(value: JsonValueType): String = {
    value match {
      case StringValueType(s) => QUOTES + s + QUOTES
      case IntValueType(i) => i.toString
    }
  }
}

trait JsonValueType

case class StringValueType(value: String) extends JsonValueType
case class IntValueType(value: Int) extends JsonValueType

case class JsonKey(name: String) {
  def :->(value: JsonValueType): (JsonKey, JsonValueType) = (this, value)

  override def toString = name
}


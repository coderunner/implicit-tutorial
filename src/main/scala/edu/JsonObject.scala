package edu

/**
 * JSON object representation.
 */
class JsonObject(properties: Map[String, Any]) {
  import JsonObject._

  /**
   * Get the value associated with the property 'name'.
   *
   * @param name the name of the property
   * @return an optional value associated with the name.
   */
  def property(name: String): Option[Any] = properties.get(name)

  /**
   * Returns an iterator over all of the JSON object properties tuples.
   *
   * @return an iterator over all of the JSON object properties tuples.
   */
  def allProperties: Iterator[(String, Any)] = properties.iterator

  /**
   * Render the JSON object as a string.
   *
   * @return the string representation of the JSON object.
   */
  def render(): String = {
    val builder = new StringBuilder(OPEN_BRACE)
    for (prop <- properties) {
      renderProperty(builder, prop)
      builder.append(COMMA)
    }
    builder.setLength(builder.length - COMMA.length)
    builder.append(CLOSING_BRACE).toString()
  }

  private def renderProperty(builder: StringBuilder, property: (String, Any)) {
    builder.append(property._1)
    builder.append(COLON)
    builder.append(renderValue(property._2))
  }

  private def renderValue(value: Any): String = {
    if (value.isInstanceOf[String]) {
      QUOTES + value.asInstanceOf[String] + QUOTES
    } else if (value.isInstanceOf[Int]) {
      value.toString
    } else {
      throw new RuntimeException("Invalid Type")
    }
  }
}

object JsonObject {
  val OPEN_BRACE = "\n"
  val COLON = " : "
  val COMMA = ",\n"
  val QUOTES = "\""
  val CLOSING_BRACE = "\n}\n"
}
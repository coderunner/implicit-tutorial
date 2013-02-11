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
      builder.append(prop._1)
      builder.append(COLON)
      builder.append(
        if (prop._2.isInstanceOf[String]) {
          "\"" + prop._2.asInstanceOf[String] + "\""
        } else if (prop._2.isInstanceOf[Int]) {
          prop._2.toString
        } else {
          throw new RuntimeException("Invalid Type")
        })
      builder.append(COMMA)
    }
    builder.setLength(builder.length - COMMA.length)
    builder.append(CLOSING_BRACE).toString()
  }
}

object JsonObject {
  val OPEN_BRACE = "{\n"
  val COLON = " : "
  val COMMA = ",\n"
  val CLOSING_BRACE = "\n}\n"
}

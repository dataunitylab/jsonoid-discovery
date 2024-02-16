package io.github.dataunitylab.jsonoid.discovery
package utils

object JsonPointer {
  def fromString(pointer: String): JsonPointer = {
    JsonPointer(
      pointer
        .stripPrefix("/")
        .split("/")
        // Reverse encoding of pointer parts
        .map(_.replace("~1", "/").replace("~0", "~"))
        .toList,
      pointer.startsWith("/")
    )
  }

  implicit val pointerToString: JsonPointer => String = _.toString
  implicit val pointerFromString: String => JsonPointer = JsonPointer.fromString
}

final case class JsonPointer(
    parts: List[String] = List.empty,
    isAbsolute: Boolean = true
) {
  override def toString: String =
    (if (isAbsolute) "/" else "") + parts
      .map(_.replace("~", "~0").replace("/", "~1"))
      .mkString("/")

  def isEmpty: Boolean = parts.isEmpty
  def nonEmpty: Boolean = parts.nonEmpty

  def append(part: String): JsonPointer = JsonPointer(parts :+ part)
}

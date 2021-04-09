package edu.rit.cs.mmior.jsonoid.discovery.schemas

import scala.reflect.ClassTag

object SchemaProperties {
  type PropertyTag[T] = ClassTag[_ <: SchemaProperty[T]]
  type PropertyMap[T] = Map[PropertyTag[T], SchemaProperty[T]]

  def empty[T]: SchemaProperties[T] = {
    SchemaProperties[T]()
  }
}

import SchemaProperties._

final case class SchemaProperties[T](
    val properties: PropertyMap[T] =
      Map.empty[PropertyTag[T], SchemaProperty[T]]
) extends Iterable[SchemaProperty[T]] {

  override def iterator: Iterator[SchemaProperty[T]] =
    properties.values.iterator

  def add[S <: SchemaProperty[T]](
      prop: S
  )(implicit tag: ClassTag[S]): SchemaProperties[T] = {
    SchemaProperties(properties + (tag -> prop))
  }

  def merge(value: T): SchemaProperties[T] = {
    val mergedProperties = properties.mapValues(_.merge(value)).map(identity(_))
    SchemaProperties(mergedProperties)
  }

  def merge(other: SchemaProperties[T]): SchemaProperties[T] = {
    val mergedProperties = properties.transform { case (key, prop) =>
      other.properties.get(key) match {
        case Some(otherProp) => prop.merge(otherProp)
        case None            => prop
      }
    }
    SchemaProperties(mergedProperties)
  }
}

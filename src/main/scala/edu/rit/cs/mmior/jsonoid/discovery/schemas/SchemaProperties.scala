package edu.rit.cs.mmior.jsonoid.discovery.schemas

import scala.reflect.ClassTag

object SchemaProperties {
  type PropertyTag[T] =
    ClassTag[_ <: SchemaProperty[T, _ <: SchemaProperty[T, _]]]
  type PropertyMap[T] =
    Map[PropertyTag[T], SchemaProperty[T, _ <: SchemaProperty[T, _]]]

  def empty[T]: SchemaProperties[T] = {
    SchemaProperties[T]()
  }
}

import SchemaProperties._

@SuppressWarnings(Array("org.wartremover.warts.Var"))
final case class SchemaProperties[T](
    var properties: PropertyMap[T] =
      Map.empty[PropertyTag[T], SchemaProperty[T, _ <: SchemaProperty[T, _]]]
) extends Iterable[SchemaProperty[T, _]] {

  override def iterator: Iterator[SchemaProperty[T, _]] =
    properties.values.iterator

  def add[S <: SchemaProperty[T, S]](
      prop: S
  )(implicit tag: ClassTag[S]): Unit = {
    properties += (tag -> prop)
  }

  def get[S <: SchemaProperty[T, S]](implicit tag: ClassTag[S]): S = {
    properties(tag).asInstanceOf[S]
  }

  def mergeValue(value: T): SchemaProperties[T] = {
    val mergedProperties =
      properties.mapValues(_.mergeValue(value)).map(identity(_))
    SchemaProperties(mergedProperties.asInstanceOf[PropertyMap[T]])
  }

  def merge(other: SchemaProperties[T]): SchemaProperties[T] = {
    val mergedProperties = properties.transform { case (key, prop) =>
      other.properties.get(key) match {
        case Some(otherProp) => prop.mergeOnlySameType(otherProp)
        case None            => prop
      }
    }
    SchemaProperties(mergedProperties.asInstanceOf[PropertyMap[T]])
  }

  def transform(
      transformer: PartialFunction[JsonSchema[_], JsonSchema[_]]
  ): SchemaProperties[T] = {
    SchemaProperties(
      properties
        .mapValues(_.transform(transformer.orElse { case x =>
          x.transformProperties(transformer)
        }))
        .asInstanceOf[PropertyMap[T]]
    )
  }
}

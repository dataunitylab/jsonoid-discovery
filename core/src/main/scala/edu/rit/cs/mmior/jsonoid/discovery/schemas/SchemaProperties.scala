package edu.rit.cs.mmior.jsonoid.discovery
package schemas

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

  def getOrNone[S <: SchemaProperty[T, S]](implicit
      tag: ClassTag[S]
  ): Option[S] = {
    if (properties.contains(tag)) {
      Some(properties(tag).asInstanceOf[S])
    } else {
      None
    }
  }

  def has[S <: SchemaProperty[T, S]](implicit tag: ClassTag[S]): Boolean = {
    properties.contains(tag)
  }

  def replaceProperty[S <: SchemaProperty[T, S]](
      prop: S
  )(implicit tag: ClassTag[S]): SchemaProperties[T] = {
    SchemaProperties(properties + (tag -> prop))
  }

  def mergeValue(
      value: T
  )(implicit er: EquivalenceRelation): SchemaProperties[T] = {
    val mergedProperties =
      properties.mapValues(_.mergeValue(value)).map(identity(_))
    SchemaProperties(mergedProperties.asInstanceOf[PropertyMap[T]])
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def merge(
      other: SchemaProperties[T],
      mergeType: MergeType = Union
  )(implicit er: EquivalenceRelation): SchemaProperties[T] = {
    var mergedProperties = properties.transform { case (key, prop) =>
      if (!prop.mergeable) {
        throw new UnsupportedOperationException("unmergeable property found")
      }

      other.properties.get(key) match {
        case Some(otherProp) => prop.mergeOnlySameType(otherProp, mergeType)
        case None            => prop
      }
    }

    // Add "other" properties which did not originally exist
    other.properties.foreach { case (tag, prop) =>
      if (!prop.mergeable) {
        throw new UnsupportedOperationException("unmergeable property found")
      }

      if (!properties.contains(tag)) {
        mergedProperties += (tag -> prop)
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

  def only(props: Seq[Class[_]]): SchemaProperties[T] = {
    val tags = props.map(ClassTag(_))
    SchemaProperties(properties.filterKeys(tags.toSet))
  }
}

package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect.ClassTag

object SchemaProperties {
  type PropertyTag[T] = ClassTag[_ <: SchemaProperty[T]]
  type PropertyMap[T] = Map[PropertyTag[T], SchemaProperty[T]]

  def empty[T]: SchemaProperties[T] = {
    SchemaProperties[T]()
  }
}

import SchemaProperties._

@SuppressWarnings(Array("org.wartremover.warts.Var"))
final case class SchemaProperties[T](
    var properties: PropertyMap[T] =
      Map.empty[PropertyTag[T], SchemaProperty[T]]
) extends Iterable[SchemaProperty[T]] {

  override def iterator: Iterator[SchemaProperty[T]] =
    properties.values.iterator

  def add[S <: SchemaProperty[T]](
      prop: S
  )(implicit tag: ClassTag[S]): Unit = {
    properties += (tag -> prop)
  }

  def get[S <: SchemaProperty[T]](implicit tag: ClassTag[S]): S = {
    properties(tag).asInstanceOf[S]
  }

  def getOrNone[S <: SchemaProperty[T]](implicit
      tag: ClassTag[S]
  ): Option[S] = {
    if (properties.contains(tag)) {
      Some(properties(tag).asInstanceOf[S])
    } else {
      None
    }
  }

  def has[S <: SchemaProperty[T]](implicit tag: ClassTag[S]): Boolean = {
    properties.contains(tag)
  }

  def replaceProperty[S <: SchemaProperty[T]](
      prop: S
  )(implicit tag: ClassTag[S]): SchemaProperties[T] = {
    SchemaProperties(properties + (tag -> prop))
  }

  def mergeValue(
      value: T
  )(implicit p: JsonoidParams): SchemaProperties[T] = {
    val mergedProperties =
      properties.mapValues(_.mergeValue(value)).map(identity(_))
    SchemaProperties(mergedProperties.asInstanceOf[PropertyMap[T]])
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def merge(
      other: SchemaProperties[T],
      mergeType: MergeType = Union
  )(implicit p: JsonoidParams): SchemaProperties[T] = {
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
      transformer: PartialFunction[(String, JsonSchema[_]), JsonSchema[_]],
      path: String
  ): SchemaProperties[T] = {
    SchemaProperties(
      properties
        .mapValues(
          _.transform(
            transformer.orElse { case x =>
              x._2.transformPropertiesWithInexactPath(transformer, false, x._1)
            },
            path
          )
        )
        .asInstanceOf[PropertyMap[T]]
    )
  }

  def only(props: Seq[Class[_]]): SchemaProperties[T] = {
    val tags = props.map(ClassTag(_))
    SchemaProperties(properties.filterKeys(tags.toSet))
  }

  def without(props: Seq[Class[_]]): SchemaProperties[T] = {
    val tags: Set[ClassTag[_]] = props.map(ClassTag(_)).toSet
    SchemaProperties(properties.filterKeys(!tags.contains(_)))
  }

  def getIncompatibleProperties[S](
      other: SchemaProperties[S],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Seq[PropertyTag[T]] = {
    properties.keySet
      .filter(tag => {
        val otherTag = tag.asInstanceOf[PropertyTag[S]]
        val prop = properties(tag)
        if (other.properties.contains(otherTag)) {
          !prop
            .isCompatibleWith(
              other.properties(otherTag).asInstanceOf[prop.S],
              recursive
            )(p)
        } else {
          true
        }
      })
      .toSeq
  }

  def isCompatibleWith[S](
      other: SchemaProperties[S],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    properties.forall { case (tag, prop) =>
      val otherTag = tag.asInstanceOf[PropertyTag[S]]
      if (other.properties.contains(otherTag)) {
        prop.isCompatibleWith(
          other.properties(otherTag).asInstanceOf[prop.S],
          recursive
        )(p)
      } else {
        true
      }
    }
  }

  def findIncompatibilities[S](
      other: SchemaProperties[S],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Seq[ClassTag[_]] = {
    properties.keySet.filter { tag =>
      val prop = properties(tag)
      val otherTag = tag.asInstanceOf[PropertyTag[S]]
      if (other.properties.contains(otherTag)) {
        !prop.isCompatibleWith(
          other.properties(otherTag).asInstanceOf[prop.S],
          recursive
        )(p)
      } else {
        // We may want to construct a default property instance here
        false
      }
    }.toSeq
  }

  def expandTo(other: SchemaProperties[T]): SchemaProperties[T] = {
    SchemaProperties(
      properties.transform { case (tag, prop) =>
        prop.expandTo(
          other
            .properties(tag.asInstanceOf[PropertyTag[T]])
            .asInstanceOf[prop.S]
        )
      }
    )
  }

  def copyWithReset(): SchemaProperties[T] = {
    SchemaProperties(properties.transform { case (tag, prop) =>
      prop.newDefault
    })
  }
}

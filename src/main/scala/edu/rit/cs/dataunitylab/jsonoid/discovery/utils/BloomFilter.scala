package io.github.dataunitylab.jsonoid.discovery
package utils

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream
}
import java.nio.charset.Charset
import java.util.Base64

import com.sangupta.bloomfilter.impl.RoaringBloomFilter

object BloomFilter {

  /** The maximum expected number of elements. */
  val ExpectedElements: Int = 100000

  /** The default false positive rate. */
  val FalsePositive: Double = 0.01

  /** Deserialize a Bloom filter object from a string. */
  def deserialize[T](bloomStr: String): BloomFilter[T] = {
    val data = Base64.getDecoder().decode(bloomStr)
    val ois = new ObjectInputStream(new ByteArrayInputStream(data))
    val filter = ois.readObject().asInstanceOf[BloomFilter[T]]
    ois.close()

    filter.filter.setCharset(Charset.defaultCharset)
    filter
  }
}

/** A serializable Bloom filter implementation based on a Roaring bitmap.
  *
  * @constructor Create a new Bloom filter
  * @param filter the underlying bitmap for the Bloom filter
  */
final case class BloomFilter[T](
    filter: RoaringBloomFilter[T] = new RoaringBloomFilter[T](
      BloomFilter.ExpectedElements,
      BloomFilter.FalsePositive
    )
) extends Serializable {

  /** Whether the Bloom filter contains a particular value. */
  def contains(value: T): Boolean = filter.contains(value)

  /** Whether the Bloom filter contains a particular serialized value. */
  def contains(value: Array[Byte]): Boolean = filter.contains(value)

  /** Whether this Bloom filter might be a subset of another. */
  def maybeSubsetOf(other: BloomFilter[_]): Boolean =
    filter.maybeSubsetOf(other.filter)

  /** Serialize this Bloom filter to a base64-encoded string. */
  def toBase64: String = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(this)
    oos.close()

    Base64.getEncoder().encodeToString(baos.toByteArray())
  }
}

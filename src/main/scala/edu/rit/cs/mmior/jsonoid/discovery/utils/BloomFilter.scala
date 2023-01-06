package edu.rit.cs.mmior.jsonoid.discovery
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
  val ExpectedElements: Int = 100000
  val FalsePositive: Double = 0.01

  def deserialize[T](bloomStr: String): BloomFilter[T] = {
    val data = Base64.getDecoder().decode(bloomStr)
    val ois = new ObjectInputStream(new ByteArrayInputStream(data))
    val filter = ois.readObject().asInstanceOf[RoaringBloomFilter[T]]
    ois.close()

    filter.setCharset(Charset.defaultCharset)
    BloomFilter(filter)
  }
}

final case class BloomFilter[T](
    filter: RoaringBloomFilter[T] = new RoaringBloomFilter[T](
      BloomFilter.ExpectedElements,
      BloomFilter.FalsePositive
    )
) extends Serializable {
  def contains(value: T): Boolean = filter.contains(value)

  def contains(value: Array[Byte]): Boolean = filter.contains(value)

  def maybeSubsetOf(other: BloomFilter[_]): Boolean =
    filter.maybeSubsetOf(other.filter)

  def toBase64(): String = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(this)
    oos.close()

    Base64.getEncoder().encodeToString(baos.toByteArray())
  }
}

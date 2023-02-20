package edu.rit.cs.mmior.jsonoid.discovery
package utils

import java.io.{
  ByteArrayOutputStream,
  IOException,
  ObjectInputStream,
  ObjectOutputStream
}
import java.util.Base64
import scala.language.implicitConversions

import com.github.prasanthj.hll.{HyperLogLog => HLL, HyperLogLogUtils}

object HyperLogLog {
  implicit def unwrapHLL(hll: HyperLogLog): HLL = hll.hll

  /** The default number of register index bits to use in the HLL data structure. */
  val DefaultRegisterIndexBits: Int = 10
}

/** A HyperLogLog data structure to track the cardinality of a set of values.
  */
class HyperLogLog extends Serializable {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  @transient var hll: HLL = HLL
    .builder()
    .setNumRegisterIndexBits(HyperLogLog.DefaultRegisterIndexBits)
    .setEncoding(HLL.EncodingType.SPARSE)
    .build()

  override def toString(): String = hll.toString()

  /** Serialize the HLL to a base-64 encoded string. */
  def toBase64(): String = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(this)
    oos.close()

    Base64.getEncoder().encodeToString(baos.toByteArray())
  }

  @throws(classOf[IOException])
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    HyperLogLogUtils.serializeHLL(out, hll)
  }

  @throws(classOf[IOException])
  private def readObject(in: ObjectInputStream): Unit = {
    in.defaultReadObject()
    hll = HyperLogLogUtils.deserializeHLL(in)
  }
}

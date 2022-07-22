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
  val DefaultRegisterIndexBits: Int = 10
}

class HyperLogLog extends Serializable {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  @transient var hll: HLL = HLL
    .builder()
    .setNumRegisterIndexBits(HyperLogLog.DefaultRegisterIndexBits)
    .setEncoding(HLL.EncodingType.SPARSE)
    .build()

  override def toString(): String = hll.toString()

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

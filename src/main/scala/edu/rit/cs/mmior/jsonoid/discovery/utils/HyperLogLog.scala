package edu.rit.cs.mmior.jsonoid.discovery
package utils

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
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

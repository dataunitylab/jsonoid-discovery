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
import com.github.sbt.jni.syntax.NativeLoader

object HyperLogLog {
  implicit def unwrapHLL(hll: HyperLogLog): HLL = hll.hll
  val DefaultRegisterIndexBits: Int = 10
}

class IntHyperLogLog extends NativeLoader("native") with Serializable {
  private val nativeHLL = init()

  @native def init(): Long
  @native def free(): Unit

  @native def add(value: Long): Unit

  @native def count(): Long

  @native def merge(other: IntHyperLogLog): Unit

  def toBase64(): String = ""

  override def finalize(): Unit = free()
}

class StringHyperLogLog extends NativeLoader("native") with Serializable {
  private val nativeHLL = init()

  @native def init(): Long
  @native def free(): Unit

  @native def add(value: String): Unit

  @native def count(): Long

  @native def merge(other: StringHyperLogLog): Unit

  def toBase64(): String = ""

  override def finalize(): Unit = free()
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

package edu.rit.cs.mmior.jsonoid.discovery
package utils

import com.github.sbt.jni.syntax.NativeLoader

abstract class HyperLogLog[T] extends NativeLoader("native") with Serializable {
  def add(value: T)
  def count(): Long
  def merge(other: HyperLogLog[T]): Unit
  def toBase64(): String = ""
}

class IntHyperLogLog extends HyperLogLog[Long] {
  private val nativeHLL = init()

  @native def init(): Long
  @native def free(): Unit

  @native override def add(value: Long): Unit
  @native override def count(): Long
  @native override def merge(other: IntHyperLogLog): Unit

  override def finalize(): Unit = free()
}

class StringHyperLogLog extends HyperLogLog[String] {
  private val nativeHLL = init()

  @native def init(): Long
  @native def free(): Unit

  @native def add(value: String): Unit
  @native def count(): Long
  @native def merge(other: StringHyperLogLog): Unit

  override def finalize(): Unit = free()
}

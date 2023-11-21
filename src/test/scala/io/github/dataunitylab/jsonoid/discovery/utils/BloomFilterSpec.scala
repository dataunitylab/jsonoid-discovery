package io.github.dataunitylab.jsonoid.discovery
package utils

class BloomFilterSpec extends UnitSpec {
  behavior of "BloomFilter"

  it should "serialize and deserialize to the same filter" in {
    val bf = BloomFilter[String]()
    bf.filter.add("foo")
    val deserialized = BloomFilter.deserialize(bf.toBase64)

    bf.maybeSubsetOf(deserialized) shouldBe (true)
    deserialized.maybeSubsetOf(bf) shouldBe (true)
  }
}

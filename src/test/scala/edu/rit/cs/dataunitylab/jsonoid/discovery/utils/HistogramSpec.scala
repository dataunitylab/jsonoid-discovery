package edu.rit.cs.dataunitylab.jsonoid.discovery
package utils

class HistogramSpec extends UnitSpec {
  behavior of "Histogram"

  it should "count only negative bins" in {
    val bins = Histogram().merge(-1.0).bins
    bins(0)._1 should equal(-1.0 +- Histogram.Tolerance)
    bins(0)._2 should equal(1)
  }

  it should "count only zero bins" in {
    val hist = Histogram().merge(0)
    hist.bins() shouldBe (List((0.0, 1)))
  }

  it should "count only positive bins" in {
    val bins = Histogram().merge(1.0).bins
    bins(0)._1 should equal(1.0 +- Histogram.Tolerance)
    bins(0)._2 should equal(1)
  }

  it should "count bins with mixed sign" in {
    val bins = Histogram().merge(-1).merge(0).merge(1).bins

    bins(0)._1 should equal(-1.0 +- Histogram.Tolerance)
    bins(0)._2 should equal(1)

    bins(1)._1 should equal(0.0 +- Histogram.Tolerance)
    bins(1)._2 should equal(1)

    bins(2)._1 should equal(1.0 +- Histogram.Tolerance)
    bins(2)._2 should equal(1)
  }

  it should "count positive values outside the bounds as anomalous" in {
    val hist = Histogram().merge(1)

    hist.isAnomalous(5) shouldBe (true)
  }

  it should "not count zero values inside the bounds as anomalous" in {
    val hist = Histogram().merge(0).merge(2)

    hist.isAnomalous(0) shouldBe (false)
  }

  it should "not count positive values inside the bounds as anomalous" in {
    val hist = Histogram().merge(0).merge(10)

    hist.isAnomalous(5) shouldBe (false)
  }

  it should "count negative values outside the bounds as anomalous" in {
    val hist = Histogram().merge(-1)

    hist.isAnomalous(-5) shouldBe (true)
  }

  it should "not count negative values inside the bounds as anomalous" in {
    val hist = Histogram().merge(-1).merge(-10)

    hist.isAnomalous(-5) shouldBe (false)
  }

  it should "count zero outside the bounds as anomalous" in {
    val hist = Histogram().merge(100)

    hist.isAnomalous(0) shouldBe (true)
  }

  it should "count not count zero as anomalous in a zero histogram" in {
    val hist = Histogram().merge(0)

    hist.isAnomalous(0) shouldBe (false)
  }
}

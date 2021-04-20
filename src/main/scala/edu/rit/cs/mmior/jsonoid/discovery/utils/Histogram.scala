package edu.rit.cs.mmior.jsonoid.discovery
package utils

object Histogram {
  val MaxBins: Int = 100
}

final case class Histogram(bins: List[(BigDecimal, BigInt)] = List.empty) {
  @SuppressWarnings(
    Array("org.wartremover.warts.Var", "org.wartremover.warts.While")
  )
  def mergeBins(
      otherBins: List[(BigDecimal, BigInt)]
  ): List[(BigDecimal, BigInt)] = {
    var newBins = otherBins

    while (newBins.size > Histogram.MaxBins) {
      // Find the pair of bins with the smallest difference
      var minDiff = BigDecimal.valueOf(Double.MaxValue)
      var minIdx = 0

      newBins.zipWithIndex.sliding(2).foreach {
        case Seq(((q1, _), i), ((q2, _), _)) =>
          val diff = q2 - q1
          if (diff < minDiff) {
            minDiff = diff
            minIdx = i
          }
      }

      val newCount = newBins(minIdx)._2 + newBins(minIdx + 1)._2
      val newBin = (
        (newBins(minIdx)._1 * BigDecimal(newBins(minIdx)._2) + newBins(
          minIdx + 1
        )._1 * BigDecimal(newBins(minIdx + 1)._2)) / BigDecimal(newCount),
        newCount
      )
      newBins =
        newBins.slice(0, minIdx) ++ List(newBin) ++ newBins.drop(minIdx + 2)
    }

    newBins
  }

  def merge(other: Histogram): Histogram = {
    Histogram(mergeBins((bins ++ other.bins).sorted))
  }
}

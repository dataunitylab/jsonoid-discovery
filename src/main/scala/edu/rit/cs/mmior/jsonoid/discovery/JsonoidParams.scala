package edu.rit.cs.mmior.jsonoid.discovery

final case class JsonoidParams(
    val er: EquivalenceRelation,
    val maxExamples: Int
) {
  def withER(newER: EquivalenceRelation): JsonoidParams = {
    JsonoidParams(newER, maxExamples)
  }

  def withMaxExamples(newMaxExamples: Int): JsonoidParams = {
    JsonoidParams(er, newMaxExamples)
  }
}

object JsonoidParams {
  implicit val defaultJsonoidParams: JsonoidParams = JsonoidParams(
    EquivalenceRelations.KindEquivalenceRelation,
    100
  )
}

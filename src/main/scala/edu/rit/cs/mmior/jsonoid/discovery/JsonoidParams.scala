package edu.rit.cs.mmior.jsonoid.discovery

final case class JsonoidParams(
    val er: EquivalenceRelation
) {
  def withER(er: EquivalenceRelation): JsonoidParams = {
    JsonoidParams(er)
  }
}

object JsonoidParams {
  implicit val defaultJsonoidParams: JsonoidParams = JsonoidParams(
    EquivalenceRelations.KindEquivalenceRelation
  )
}

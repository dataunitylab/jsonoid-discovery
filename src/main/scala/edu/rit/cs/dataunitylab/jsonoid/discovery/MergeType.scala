package io.github.dataunitylab.jsonoid.discovery

/** The type of merge to be used when combining schemas. */
sealed trait MergeType {}

/** Merge schemas by taking their union, producing a schema that will accept the
  *  all documents accepted by either schema.
  */
case object Union extends MergeType {}

/** Merge schemas by taking their intersection, producing a schema that will
  *  accept the all documents accepted by both schemas.
  */
case object Intersect extends MergeType {}

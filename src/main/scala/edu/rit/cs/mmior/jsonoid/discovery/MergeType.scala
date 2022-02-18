package edu.rit.cs.mmior.jsonoid.discovery

sealed trait MergeType {}
case object Union extends MergeType {}
case object Intersect extends MergeType {}

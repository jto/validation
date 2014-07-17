package play.api.data.mapping

package object delimited {
  type Delimited = Array[String]
  type DelimitedVA[O] = Validation[(IdxPathNode, Seq[ValidationError]), O]
}
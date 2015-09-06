package jto.validation

package object delimited {
  type Delimited = Array[String]
  type DelimitedVA[O] = Validation[(IdxPathNode, Seq[ValidationError]), O]
}

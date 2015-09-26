package jto.validation

package object delimited {
  type Delimited = Array[String]
  type DelimitedVA[O] = Validated[(IdxPathNode, Seq[ValidationError]), O]
}

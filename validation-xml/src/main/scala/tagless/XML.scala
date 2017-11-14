package jto.validation
package v3.tagless

import scala.xml.{NodeSeq, MetaData}

package object xml {
  type XML = (MetaData, NodeSeq)
}

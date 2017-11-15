package jto.validation
package v3.tagless
package forms

import jto.validation.forms._

trait FormGrammar[K[_, _]] extends Grammar[PM.PM, K] {
  type I = PM.PM
}

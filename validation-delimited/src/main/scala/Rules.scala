package jto.validation.delimited

import jto.validation._

/**
  * Rules for parsing/validating/transforming Array[String] as typically returned from CSV parsers.
  *
  * {{
  *   case class Contact(name: String, email: String, birthday: Option[LocalDate])
  *
  *   val contactReads = From[Delimited] { __ => (
  *     (__ \ 0).read[String] and
  *     (__ \ 1).read(email) and
  *     (__ \ 2).read(optionR[LocalDate](equalTo("N/A")))
  *   )(Contact)}
  *
  *   val csv1 = "Ian Hummel,ian@example.com,1981-07-24".split(",")
  *   val csv2 = "Jane Doe,jane@example.com,N/A".split(",")
  *
  *   contactReads.validate(csv1) // returns Valid(Contact("Ian Hummel", "ian@example.com", Some(new LocalDate(1981, 7, 24))))
  *   contactReads.validate(csv2) // returns Valid(Contact("Jane Doe", "jane@example.com", None))
  * }}
  */
trait Rules extends DefaultRules[Delimited] with ParsingRules {
  import scala.language.implicitConversions

  /**
    * Extract the value at a given index, transforming it into a given type.
    *
    * @param p  An index into the array
    * @param r  A Rule for converting the value from String to O
    * @tparam O The desired type for the value
    * @return   Invalid if the index is out of bounds or the Path was not an IdxPathNode
    */
  implicit def pick[O](p: Path)(
      implicit r: RuleLike[String, O]): Rule[Delimited, O] =
    Rule[Delimited, String] { delimited =>
      p.path match {
        case IdxPathNode(i) :: t if i < delimited.length => Valid(delimited(i))
        case _ => Invalid(Seq(Path -> Seq(ValidationError("error.required"))))
      }
    }.andThen(r)

  /**
    * By default, the empty string "" will be considered as None for Option reads
    */
  private val isEmpty = validateWith[String]("error.present") { _.isEmpty }

  /**
    * Read an optional value using the specified value/rules to determine what is considered None vs what is Some(_).
    *
    * @param noneValues Rules for determining if a value should be None
    * @param pick       Function to extract a value from a given index
    * @param coerce     Coerce the value from String to type O
    * @tparam O         The desired type for the value
    * @return           The optional value
    */
  def optionR[O](noneValues: RuleLike[String, String]*)(
      implicit pick: Path => RuleLike[Delimited, String],
      coerce: RuleLike[String, O]): Path => Rule[Delimited, Option[O]] =
    myOpt[O](coerce, noneValues: _*)

  /**
    * Function for creating a mapping from indexes to [[Rule]]s which read optional values from an Array[String].
    *
    * @param coerce     Coerce the value from String to type O
    * @param noneValues Rules for determining if a value should be None
    * @param pick       Function to extract a value from a given index
    * @tparam O         The desired type for the value
    * @return           The optional value
    */
  private def myOpt[O](
      coerce: => RuleLike[String, O], noneValues: RuleLike[String, String]*)(
      implicit pick: Path => RuleLike[Delimited, String]) =
    (path: Path) =>
      Rule[Delimited, Option[O]] { delimited =>
        val isNone =
          not(noneValues.foldLeft(Rule.zero[String])(_ andThen not(_)))
            .map(_ => None)
        val v =
          (pick(path).validate(delimited).map(Some.apply) orElse Valid(None))
        Validated.fromEither(
            v.toEither.right.flatMap {
              case None => Right(None)
              case Some(i) =>
                isNone
                  .orElse(Rule.toRule(coerce).map[Option[O]](Some.apply))
                  .validate(i)
                  .toEither
            }
        )
    }

  /**
    * An implicit defining a default Option reader.  Uses "" as the empty value.
    *
    * @param p      An index into the array
    * @param pick   Function to extract a value from a given index
    * @param coerce Coerce the value from String to type O
    * @tparam O     The desired type for the value
    * @return       The optional value
    */
  implicit def ooo[O](
      p: Path)(implicit pick: Path => RuleLike[Delimited, String],
               coerce: RuleLike[String, O]): Rule[Delimited, Option[O]] =
    optionR(isEmpty)(pick, coerce)(p)
}

object Rules extends Rules

package jto.validation
package forms

import shapeless.tag, tag.@@
import scala.util.parsing.combinator.RegexParsers

/**
  * Play provides you a `Map[String, Seq[String]]` (aliased as `UrlFormEncoded`) in request body for urlFormEncoded requests.
  * It's generally a lot more convenient to work on `Map[Path, String]` to define Rules.
  * This object contains methods used to convert `Map[String, Seq[String]]` <-> `Map[Path, String]`
  * @note We use the alias `UrlFormEncoded`, which is just a `Map[String, Seq[String]]`
  */
object PM {

  /**
    * A parser converting a key of a Map[String, [Seq[String]]] to a Path instance
    * `foo.bar[0].baz` becomes `Path \ "foo" \ "bar" \ 0 \ "baz"`
    */
  object PathParser extends RegexParsers {
    override type Elem = Char
    def int = """\d""".r ^^ { _.toInt }
    def idx = "[" ~> int <~ "]" ^^ { IdxPathNode(_) }
    def key = rep1(not("." | idx) ~> ".".r) ^^ { ks =>
      KeyPathNode(ks.mkString)
    }
    def node = key ~ opt(idx) ^^ { case k ~ i => k :: i.toList }
    def path = (opt(idx) ~ repsep(node, ".")) ^^ {
      case i ~ ns => Path(i.toList ::: ns.flatten)
    }

    def parse(s: String) =
      parseAll(path, new scala.util.parsing.input.CharArrayReader(s.toArray))
  }

  type PM = Map[Path, String]

  /**
    * Find a sub-Map of all the elements at a Path starting with `path`
    * @param path The prefix to look for
    * @param data The map in which you want to lookup
    * @return a sub Map. If no key of `data` starts with `path`, this map will be empty
    */
  def find(path: Path)(data: PM): PM = data.flatMap {
    case (p, v) if p.path.startsWith(path.path) =>
      Map(Path(p.path.drop(path.path.length)) -> v)
    case _ =>
      Map.empty[Path, String]
  }

  /**
    * Apply `f` to all the keys of `m`
    */
  def repathPM(m: PM, f: Path => Path): PM = m.map { case (p, v) => f(p) -> v }

  /**
    * Apply `f` to all the keys of `m`
    */
  def repath(m: UrlFormEncoded, f: Path => Path): UrlFormEncoded =
    toM(repathPM(toPM(m), f))

  /**
    * Convert a Map[String, Seq[String]] to a Map[Path, Seq[String]]
    */
  def toPM(m: UrlFormEncoded): PM =
    m.toSeq.flatMap {
      case (p, vs) =>
        if (p.endsWith("[]")) {
          vs.zipWithIndex.map {
            case (v, i) => (asPath(p.dropRight(2)) \ i) -> v
          }
        } else {
          vs.headOption.map { asPath(p) -> _ }.toSeq
        }
    }.toMap

  /**
    * Convert a Map[Path, Seq[String]] to a Map[String, Seq[String]]
    */
  def toM(m: PM): UrlFormEncoded =
    m.map { case (p, v) => asKey(p) -> Seq(v) }

  private def asNodeKey(n: PathNode): String = n match {
    case IdxPathNode(i) => s"[$i]"
    case KeyPathNode(k) => k
  }

  /**
    * Convert a Path to a String key
    * @param p The path to convert
    * @return A String representation of `p`
    */
  def asKey(p: Path): String =
    p.path.headOption.toList.map(asNodeKey).mkString ++ p.path.tail
      .foldLeft("") {
      case (path, n @ IdxPathNode(_)) => path + asNodeKey(n)
      case (path, n @ KeyPathNode(_)) => path + "." + asNodeKey(n)
    }

  /**
    * Convert a String key to a Path using `PathParser`
    * @param k The String representation of path to convert
    * @return a `Path`
    */
  def asPath(k: String): Path = PathParser.parse(k) match {
    case PathParser.Failure(m, _) =>
      throw new RuntimeException(s"Invalid field name $k: $m")
    case PathParser.Error(m, _) =>
      throw new RuntimeException(s"Invalid field name $k: $m")
    case PathParser.Success(r, _) => r
  }
}

/**
  * This object provides Rules for Map[String, Seq[String]]
  */
trait Rules extends DefaultRules[PM.PM] with ParsingRules {
  import PM._

  implicit def mapR[O](
      implicit r: RuleLike[Seq[String], O]): Rule[PM, Map[String, O]] =
    super.mapR[Seq[String], O](r, Rule.zero[PM].map { toM(_).toSeq })

  private val isEmpty = validateWith[PM]("validation.empty") { pm =>
    pm.filter { case (_, vs) => !vs.isEmpty }.isEmpty
  }

  implicit def optionR[O](
      implicit pick: Path => RuleLike[PM, PM],
      coerce: RuleLike[PM, O]): Path => Rule[PM, Option[O]] =
    opt(coerce, isEmpty)(pick, Rule.zero)

  def optionR[J, O](r: => RuleLike[J, O], noneValues: RuleLike[PM, PM]*)(
      implicit pick: Path => RuleLike[PM, PM],
      coerce: RuleLike[PM, J]): Path => Rule[UrlFormEncoded, Option[O]] =
    path => {
      val nones = isEmpty +: noneValues
      val o = opt[J, O](r, nones: _*)(pick, coerce)(path)
      Rule.zero[UrlFormEncoded].map(toPM).andThen(o)
    }

  implicit def parseString[O](implicit r: RuleLike[String, O]): Rule[PM, O] @@ Root = {
    val find = Rule[Option[String], String] {
      _.map(x => Valid(Path -> x)).getOrElse(
          Invalid(Seq(Path -> Seq(ValidationError("error.required")))))
    }
    tag[Root](Rule.zero[PM].map(_.get(Path)).andThen(find).andThen(r))
  }

  implicit def inArray[O: scala.reflect.ClassTag](
      implicit r: RuleLike[Seq[PM], Array[O]]): Path => Rule[PM, Array[O]] =
    inT[O, Traversable](Rule.toRule(r).map(_.toTraversable))(_).map(_.toArray)

  implicit def inT[O, T[_] <: Traversable[_]](
      implicit r: RuleLike[Seq[PM], T[O]]): Path => Rule[PM, T[O]] =
    path =>
      pickInPM(path)(Rule.zero)
        .orElse(Rule[PM, PM](_ => Valid(path -> Map.empty)))
        .map { pm =>
          val (root, others) = pm.partition(_._1 == Path)
          val arrays = others.toSeq.flatMap {
            case (Path(IdxPathNode(i) :: Nil) \: t, v) => Seq(i -> Map(t -> v))
            case _ => Nil
          }.groupBy(_._1).toSeq.sortBy(_._1).map {
            case (_, pms) =>
              pms.map(_._2).foldLeft(Map.empty[Path, String]) { _ ++ _ }
          }
          (root +: arrays).filter(!_.isEmpty)
        }
        .andThen(r)

  implicit def pickInPM[O](p: Path)(implicit r: RuleLike[PM, O]): Rule[PM, O] =
    Rule[PM, PM] { pm =>
      Valid(p -> PM.find(p)(pm))
    }.andThen(r)

  // Convert Rules exploring PM, to Rules exploring UrlFormEncoded
  implicit def convertToInM[O](p: Path)(
      implicit r: Path => RuleLike[PM, O]): Rule[UrlFormEncoded, O] =
    Rule.zero[UrlFormEncoded].map(toPM).andThen(r(p))

  implicit def convertRule[O](
      implicit r: RuleLike[UrlFormEncoded, O]): Rule[PM, O] =
    Rule.zero[PM].map(toM).andThen(r)
}

object Rules extends Rules

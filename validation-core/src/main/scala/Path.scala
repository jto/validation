package jto.validation

sealed trait PathNode
final case class KeyPathNode(key: String) extends PathNode {
  override def toString = key
}

final case class IdxPathNode(idx: Int) extends PathNode {
  override def toString = s"[$idx]"
}

object \: {
  def unapply(path: Path): Option[(Path, Path)] = {
    path match {
      case Path(n :: ns) => Some((Path \ n) -> Path(ns))
      case Path(Nil) => None
    }
  }
}

case object Path extends Path(Nil) {
  def apply(path: String) = new Path(KeyPathNode(path) :: Nil)
  def apply(path: List[PathNode] = Nil) = new Path(path)
  def unapply(p: Path): Option[List[PathNode]] = Some(p.path)
}

class Path(val path: List[PathNode]) {

  def \(key: String): Path = this \ KeyPathNode(key)
  def \(idx: Int): Path = this \ IdxPathNode(idx)
  def \(child: PathNode): Path = Path(path :+ child)

  /**
    * Aggregate 2 paths
    * {{{
    *   (Path \ "foo" \ "bar") .andThen(Path \ "baz") == (Path \ "foo" \ "bar" \ "baz")
    * }}}
    */
  def compose(p: Path): Path = Path(this.path ++ p.path)
  def ++(other: Path) = this compose other

  class Deferred[I] private[Path](reader: Reader[I]) {
    def apply[J, O](sub: => RuleLike[J, O])(
        implicit r: Path => RuleLike[I, J]): Rule[I, O] =
      reader.read(sub)
  }

  def from[I] = new Deferred(Reader[I](this))

  def read[I, J, O](sub: => RuleLike[J, O])(
      implicit r: Path => RuleLike[I, J]): Rule[I, O] =
    Reader[I](this).read(sub)

  def read[I, O](implicit r: Path => RuleLike[I, O]): Rule[I, O] =
    Reader[I](this).read[O]

  /**
    * Creates a Writes the serialize data to the desired output type
    * {{{
    *   val contact = Contact("Julien", "Tournay")
    *   implicit def contactWrite = (Path \ "firstname").write[String, UrlFormEncoded]
    *   contactWrite.writes(contact) shouldBe Map("firstname" -> "Julien")
    * }}}
    */
  def write[I, O](implicit w: Path => WriteLike[I, O]): Write[I, O] =
    Writer[O](this).write(w)

  /**
    * Creates a Writes the serialize data to the desired output type using a provided format.
    * * {{{
    *   val w = (Path \ "date").write(date("yyyy-MM-dd""))
    *   w.writes(new Date()) == Json.obj("date" -> "2013-10-3")
    * }}}
    */
  def write[I, J, O](format: => WriteLike[I, J])(
      implicit w: Path => WriteLike[J, O]): Write[I, O] =
    Writer[O](this).write(format)

  override def toString = this.path match {
    case Nil => "/"
    case hs =>
      hs.foldLeft("") {
        case (path, IdxPathNode(i)) => path + s"[$i]"
        case (path, KeyPathNode(k)) => path + "/" + k
      }
  }

  override def hashCode = path.hashCode
  override def equals(o: Any) = {
    if (canEqual(o)) {
      val j = o.asInstanceOf[Path]
      this.path == j.path
    } else
      false
  }
  def canEqual(o: Any) = o.isInstanceOf[Path]
}

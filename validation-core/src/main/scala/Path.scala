package jto.validation

import scala.reflect.ClassTag

sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode {
  override def toString = key
}

case class IdxPathNode(idx: Int) extends PathNode {
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

class Path(val path: List[PathNode]) { self =>
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

  class FromCurried[I] {
    def apply[O](r: Rule[I, O])(
        implicit a: At[Rule[I, ?]], t: ClassTag[O]): Rule[I, O] =
      As1[Rule[I, ?]](self).as(t, r)
  }

  def from[I] = new FromCurried[I]()

  def read[I, O](
      implicit a: At[Rule[I, ?]], r: Rule[I, O], t: ClassTag[O]): Rule[I, O] =
    As1[Rule[I, ?]](this).as(t, r)

  def write[I, O](implicit a: At[Write[?, O]],
                  w: Write[I, O],
                  t: ClassTag[I]): Write[I, O] =
    As1[Write[?, O]](this).as[I]

  def write[I, O](w: Write[I, O])(
      implicit a: At[Write[?, O]], t: ClassTag[I]): Write[I, O] =
    As1[Write[?, O]](this).as(t, w)

  override def toString = this.path match {
    case Nil => "/"
    case hs =>
      hs.foldLeft("") {
        case (path, IdxPathNode(i)) => path + s"[$i]"
        case (path, KeyPathNode(k)) => path + "/" + k
      }
  }

  override def hashCode = path.hashCode

  override def equals(o: Any) =
    o match {
      case p: Path => this.path == p.path
      case _ => false
    }
}

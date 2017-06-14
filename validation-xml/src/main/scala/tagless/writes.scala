package jto.validation
package v3.tagless
package xml

import types.flip
import scala.xml._
import jto.validation.xml.{Writes => W}

trait WritesGrammar
  extends XmlGrammar[Node, flip[Write]#λ]
  with WriteConstraints
  with WritesTypeclasses[Node] {

  self =>

  type Out = Node
  type Sub = Elem
  type P = WritesGrammar

  def mapPath(f: Path => Path): P =
    new WritesGrammar {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Elem] =
        self.at(f(p))(k)
    }

  override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: Node]]): Write[A, Elem] =
    Write { i =>
      val in = k.writes(i).getOrElse(Group(Nil))

      @annotation.tailrec
      def go(child: Elem, path: List[PathNode]): Elem =
        path match {
          case Nil =>
            child
          case KeyPathNode(key) :: xs =>
            go(Elem(null, key, Null, TopScope, false, child), xs)
          case IdxPathNode(_) :: _ =>
            throw new RuntimeException("cannot write an attribute to a node with an index path")
        }

      if(p.path.isEmpty)
        throw new RuntimeException("cannot write a node at no path")

      val KeyPathNode(root) :: tail = p.path.reverse
      go(Elem(null, root, Null, TopScope, false, in), tail)
    }

  def opt[A](implicit K: Write[A, _ >: Out <: Node]): Write[Option[A], Option[Node]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: Node]): Write[A, Option[Node]] =
    Write { a =>
      Option(K.writes(a))
    }

  def toGoal[Repr, A]: Write[Repr, Out] => Write[Goal[Repr, A], Out] =
    _.contramap{ _.value }

  private def txt[A](w: Write[A, String]) = Write[A, Node] { i => Text(w.writes(i)) }

  implicit def bigDecimal = txt(W.bigDecimalW)
  implicit def boolean = txt(W.booleanW)
  implicit def double = txt(W.doubleW)
  implicit def float = txt(W.floatW)
  implicit def int = txt(W.intW)
  implicit def jBigDecimal = txt(Write(_.toString))
  implicit def long = txt(W.longW)
  implicit def short = txt(W.shortW)
  implicit def string = txt(Write.zero)

  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: Node]): Write[Array[A], Node] = seq(k).contramap(_.toSeq)
  implicit def list[A](implicit k: Write[A, _ >: Out <: Node]): Write[List[A], Node] = seq(k).contramap(_.toSeq)
  implicit def map[A](implicit k: Write[A, _ >: Out <: Node]): Write[Map[String, A], Node] = ???
  implicit def seq[A](implicit k: Write[A, _ >: Out <: Node]): Write[Seq[A], Node] = Write[Seq[A], Node] { as => Group(as.map(k.writes)) }
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: Node]): Write[Traversable[A], Node] = seq(k).contramap(_.toSeq)

  protected def iMonoid =
    new cats.Monoid[Out] {
      def combine(x: Out, y: Out): Out = Group(x ++ y)
      def empty = Group(Nil)
    }

  // def withAttr[A, B](key: String, attrK: Write[B, Option[Node]])(K: Write[A, Elem]): Write[(A, B), Elem] =
  //   Write { case (a, b) =>
  //     val elem = K.writes(a)
  //     elem.copy(attributes = elem.attributes.append(new UnprefixedAttribute(key, attrK.writes(b), Null)))
  //   }
}

object WritesGrammar extends WritesGrammar


sealed trait XML {
  def apply(n: NodeSeq): XML = XML.Just(n)

  def toFun: Elem => Elem =
    this match {
      case XML.Group(xs) =>
        xs.map(_.toFun).foldLeft[Elem => Elem](identity)(_ andThen _)
      case XML.Just(xml) =>
        el => el.copy(child = el.child ++ xml)
      case XML.Text(txt) =>
        el => el.copy(child = el.child ++ Text(txt))
      case XML.Attr(name, value) =>
        el => el.copy(attributes = el.attributes.append(new UnprefixedAttribute(name, value, Null)))
      case XML.At(location, value) => el =>
        val f = value.toFun
        val es =
          location.path.map {
            case KeyPathNode(key) =>
              Elem(null, key, Null, TopScope, false)
            case IdxPathNode(_) =>
              throw new RuntimeException("cannot write an attribute to a node with an index path")
          }

        val xml =
          es.lastOption.map { e =>
            val last = f(e)
            es.init.reverse.foldLeft(last){ (e, els) => els.copy(child = els.child ++ e) }
          }.getOrElse(NodeSeq.Empty)

        el.copy(child = el.child ++ xml)
    }
}

object XML {
  final case class Group(values: List[XML.At]) extends XML {
    def build: NodeSeq =
      values.map(_.build).foldLeft(NodeSeq.Empty)(_ ++ _)
  }

  final case class Just(value: NodeSeq) extends XML
  final case class Text(value: String) extends XML
  final case class Attr(name: String, value: String) extends XML
  final case class At(location: Path, value: XML) extends XML {
    def build: NodeSeq =
      location.path match {
        case Nil =>
          ??? // should be impossible
        case KeyPathNode(key) :: Nil =>
          val root = Elem(null, key, Null, TopScope, false)
          value.toFun(root)
        case KeyPathNode(key) :: p =>
          val root = Elem(null, key, Null, TopScope, false)
          At(Path(p), value).toFun(root)
        case IdxPathNode(_) :: _ =>
          throw new RuntimeException("cannot write an attribute to a node with an index path")
      }
  }
}

trait WritesGrammar2 extends Grammar[XML, flip[Write]#λ]
  with WriteConstraints
  with WritesTypeclasses[XML] {

  self =>

  type Out = XML.Group
  type P = WritesGrammar2

  import shapeless.tag.@@
  def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: XML]]): Write[A, XML.Group] =
    Write { a =>
      val xml = k.writes(a).getOrElse(XML.Group(Nil))
      XML.Group(XML.At(p, xml) :: Nil)
    }

  def opt[A](implicit K: Write[A, _ >: Out <: XML]): Write[Option[A], Option[_ >: Out <: XML]] =
    Write { _.map(K.writes) }

  def req[A](implicit K: Write[A, _ >: Out <: XML]): Write[A, Option[_ >: Out <: XML]] =
    Write { a => Option(K.writes(a)) }

  def mapPath(f: Path => Path): P =
    new WritesGrammar2 {
      override def at[A](p: Path)(k: => Write[A, Option[_ >: Out <: XML]]): Write[A, XML.Group] =
        self.at(f(p))(k)
    }

  private def txt[A](w: Write[A, String]): Write[A, XML.Text] @@ Root =
    Write { i => XML.Text(w.writes(i)) }

  implicit def bigDecimal = txt(W.bigDecimalW)
  implicit def boolean = txt(W.booleanW)
  implicit def double = txt(W.doubleW)
  implicit def float = txt(W.floatW)
  implicit def int = txt(W.intW)
  implicit def jBigDecimal = txt(Write(_.toString))
  implicit def long = txt(W.longW)
  implicit def short = txt(W.shortW)
  implicit def string = txt(Write.zero)

  implicit def map[A](implicit k: Write[A, _ >: Out <: XML]): Write[Map[String, A], XML] = ???
  implicit def seq[A](implicit k: Write[A, _ >: Out <: XML]): Write[Seq[A], XML] = ???
  implicit def array[A: scala.reflect.ClassTag](implicit k: Write[A, _ >: Out <: XML]): Write[Array[A], XML] = seq(k).contramap(_.toSeq)
  implicit def list[A](implicit k: Write[A, _ >: Out <: XML]): Write[List[A], XML] = seq(k).contramap(_.toSeq)
  implicit def traversable[A](implicit k: Write[A, _ >: Out <: XML]): Write[Traversable[A], XML] = seq(k).contramap(_.toSeq)

  def toGoal[Repr, A]: Write[Repr,Out] => Write[Goal[Repr, A], Out] =
    _.contramap{ _.value }

  protected def iMonoid: cats.Monoid[Out] =
    new cats.Monoid[Out] {
      def combine(x: Out, y: Out): Out =
        XML.Group(x.values ++ y.values)
      def empty =
        XML.Group(Nil)
    }

  def attr[A](label: String)(implicit w: Write[A, XML.Text]): Write[A, XML] =
    Write{ a =>
      XML.Attr(label, w.writes(a).value)
    }
}

object WritesGrammar2 extends WritesGrammar2 {
  def test1 = at(Path \ "foo" \ "bar")(req[String])

  def test2 = at(Path \ "foo" \ "bar")(opt[String])

  def test3 =
    at(Path \ "bar")(req[String]) ~:
    at(Path \ "foo")(opt[String]) ~:
    knil

  def test4 =
    at(Path \ "foo" \ "bar")(req(attr[Int]("id"))) ~:
    at(Path \ "baz")(req[String]) ~:
    knil

  def test5 =
    at(Path \ "foo" \ "bar"){
      req(attr[Int]("id")) ~: knil
    }
}

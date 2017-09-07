package jto.validation
package v3.tagless
package xml

import scala.xml.{Group => _, _}

sealed trait XML

object XML {
  final case class Group(values: List[XML.At]) extends XML {
    def build: NodeSeq =
      values.map(_.build).foldLeft(NodeSeq.Empty)((a, b) => a ++ b)
  }

  final case class Text(value: String) extends XML
  final case class Attr(name: String, value: String) extends XML
  final case class XList(values: List[XML]) extends XML

  final case class At(location: KeyPathNode, xml: XML) extends XML {
    def build: NodeSeq = {

      @inline def go(value: XML): NodeSeq =
        value match {
          case Text(txt) =>
            NodeSeq.Empty ++ Elem(null, location.key, Null, TopScope, false, scala.xml.Text(txt))
          case Attr(n, v) =>
            NodeSeq.Empty ++ Elem(null, location.key, new UnprefixedAttribute(n, v, Null), TopScope, false)
          case Group(vs) =>
            val children = vs.map {
              _.build
            }.foldLeft(NodeSeq.Empty)(_ ++ _)

            if(children.isEmpty) {
              NodeSeq.Empty
            } else {
              val el = Elem(null, location.key, Null, TopScope, false, children.theSeq :_*)
              NodeSeq.Empty  ++ el
            }

          case at @ XList(xs) =>
            xs.map(go).foldLeft(NodeSeq.Empty)(_ ++ _)
          case at @ At(_, _) =>
            val children = at.build.theSeq
            if(children.isEmpty)
              NodeSeq.Empty
            else
              NodeSeq.Empty ++ Elem(null, location.key, Null, TopScope, false, at.build.theSeq:_*)
        }

      go(xml)
    }
  }
}
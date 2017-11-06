package jto.validation
package v3.tagless
package xml

import scala.xml._

object XMLTestCases extends TestCases[NodeSeq] {

  override val base = new base {
    def id = <id>1</id>

    def emptyObj = <root></root>
    def noInfo = NodeSeq.Empty
    def smthFalse = <issmth>true</issmth>
    def smthTrue = <issmth>false</issmth>

    def info =
      <label>Personal</label>
      <email>fakecontact@gmail.com</email>
      <phones>
        <phone label="mobile">01.23.45.67.89</phone>
        <phone label="home">98.76.54.32.10</phone>
      </phones>

    def contacts =
      <contacts label="Personal">
        <label>Personal</label>
        <email>fakecontact@gmail.com</email>
        <phones label="mobile">01.23.45.67.89</phones>
        <phones label="home">98.76.54.32.10</phones>
      </contacts>

    def jto =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        {info}
      </person>

    def valid =
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        <informations>{info}</informations> ++ {contacts}

    def invalid =
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        <informations label="">
          <label/>
          <email>fakecontactgmail.com</email>
          <phones>
            <phone label="mobile">01.02</phone>
            <phone label="home">01.02</phone>
          </phones>
        </informations>
        <contacts>
          <label/>
          <email>fakecontact@gmail.com</email>
          <phones>
            <phone label="mobile">01.23.45.67.89</phone>
            <phone label="home">98.76.54.32.10</phone>
          </phones>
        </contacts>
  }

  val int = new int {
    def ok = <n>4</n> ++ NodeSeq.Empty
    def foo = <n>foo</n> ++ NodeSeq.Empty
    def float = <n>4.5</n> ++ NodeSeq.Empty
    def noOK = <n><o>4</o></n> ++ NodeSeq.Empty
    def noFoo = <n><o>foo</o></n> ++ NodeSeq.Empty
    def nopOK = <n><o><p>4</p></o></n> ++ NodeSeq.Empty
    def nopFoo = <n><o><p>foo</p></o></n> ++ NodeSeq.Empty
  }

  val boolean = new boolean {
    def ok = <n>true</n> ++ NodeSeq.Empty
    val foo = int.foo
  }

  val string = new string {
    val foo = int.foo
    def foos = int.foo
    def _42 = <n>42</n> ++ NodeSeq.Empty
    def onFoo = <o><n>foo</n></o> ++ NodeSeq.Empty
  }

  val option = new option {
    def nNull = <root><n/></root> ++ NodeSeq.Empty
    def fooBar = <foo>bar</foo> ++ NodeSeq.Empty
    def nBar = <n>bar</n> ++ NodeSeq.Empty
    def none = NodeSeq.Empty
  }

  val seq = new seq {
    def foos = <n>foo</n> ++ NodeSeq.Empty

    def fooBars =
        <foo>bar</foo>

    def foofoobars =
        <foo><foo>bar</foo></foo> ++ NodeSeq.Empty

    def ns =
      <n>foo</n>
      <n></n>


    def ints =
      <n>1</n>
      <n>2</n>
      <n>3</n>

    def paf = <root><n>paf</n></root>
    def mixed =
      <root>
        <n>
          <node>foo</node>
          <node>2</node>
        </n>
      </root>
  }

  val map = new map {
    def foobar = <n><foo>bar</foo></n>
    def ints = <n><foo>4</foo><bar>5</bar></n>
    def mixed = <n><foo>4</foo><bar>frack</bar></n>
  }

  val password = new password {
    def ok =
      <login>Alice</login>
      <password>s3cr3t</password>
      <verify>s3cr3t</verify>


    def empty =
      <login>Alice</login>
      <password>s3cr3t</password>
      <verify/>


    def err =
      <login>Alice</login>
      <password>s3cr3t</password>
      <verify>bam</verify>

  }

  val subclasses = new subclasses {
    def b = <name>B</name><foo>4</foo>
    def c = <name>C</name><bar>6</bar>
    def e = <name>E</name><eee>6</eee>
  }

  val rec = new rec {
    def bobAndFriends =
      <person>
        <name>bob</name>
        <friends>
          <friend>
            <name>tom</name>
            <friends/>
          </friend>
        </friends>
      </person>

    def bobAndFriend =
      <person>
        <name>bob</name>
        <friend>
          <name>tom</name>
        </friend>
      </person>
  }

}
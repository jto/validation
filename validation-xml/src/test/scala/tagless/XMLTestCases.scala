package jto.validation
package v3.tagless
package xml

import scala.xml._

object XMLTestCases extends TestCases[NodeSeq] {

  override val base = new base {
    def id = <id>1</id>

    def emptyObj = <root></root>
    def noInfo = <phones></phones>
    def smthFalse = <issmth>true</issmth>
    def smthTrue = <issmth>false</issmth>

    def info =
      <informations label="Personal">
        <label>Personal</label>
        <email>fakecontact@gmail.com</email>
        <phones>
          <phone label="mobile">01.23.45.67.89</phone>
          <phone label="home">98.76.54.32.10</phone>
        </phones>
      </informations>

    def contacts =
      <contacts label="Personal">
        <contact>
          <label>Personal</label>
          <email>fakecontact@gmail.com</email>
          <phones>
            <phone label="mobile">01.23.45.67.89</phone>
            <phone label="home">98.76.54.32.10</phone>
          </phones>
        </contact>
      </contacts>

    def jto =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        ${info}
      </person>

    def valid =
      <person>
        <firstname>Julien</firstname>
        <lastname>Tournay</lastname>
        <age>27</age>
        ${info}
        ${contacts}
      </person>

    def invalid =
      <person>
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
        <contacts label="">
        <contact>
          <label/>
          <email>fakecontact@gmail.com</email>
          <phones>
            <phone label="mobile">01.23.45.67.89</phone>
            <phone label="home">98.76.54.32.10</phone>
          </phones>
        </contact>
      </contacts>
      </person>
  }

  val int = new int {
    def ok = <n>4</n>
    def foo = <n>foo</n>
    def float = <n>4.5</n>
    def noOK = <n><o>4</o></n>
    def noFoo = <n><o>foo</o></n>
    def nopOK = <n><o><p>4</p></o></n>
    def nopFoo = <n><o><p>foo</p></o></n>
  }

  val boolean = new boolean {
    def ok = <root><n>true</n></root>
    val foo = int.foo
  }

  val string = new string {
    val foo = int.foo
    def foos = int.foo
    def _42 = <root><n>42</n></root>
    def onFoo = <root><o><n>foo</n></o></root>
  }

  val option = new option {
    def nNull = <root><n/></root>
    def fooBar = <root><foo>bar</foo></root>
    def nBar = <root><n>bar</n></root>
    def none = <root><n/></root>
  }

  val seq = new seq {
    def foos =
      <root>
        <n>
          <node>foo</node>
        </n>
      </root>

    def fooBars =
      <root>
        <foo>
          <node>bar</node>
        </foo>
      </root>

    def foofoobars =
      <root>
        <foo>
          <foo>
            <node>bar</node>
          </foo>
        </foo>
      </root>

    def ns =
      <root>
        <n>
          <node>foo</node>
          <node></node>
        </n>
      </root>

    def ints =
      <root>
        <n>
          <node>1</node>
          <node>2</node>
          <node>3</node>
        </n>
      </root>

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
    def foobar = <root><n><foo>bar</foo></n></root>
    def ints = <root><n><foo>4</foo><bar>5</bar></n></root>
    def mixed = <root><n><foo>4</foo><bar>frack</bar></n></root>
  }

  val password = new password {
    def ok =
      <root>
        <login>Alice</login>
        <password>s3cr3t</password>
        <verify>s3cr3t</verify>
      </root>

    def empty =
      <root>
        <login>Alice</login>
        <password>s3cr3t</password>
        <verify/>
      </root>

    def err =
      <root>
        <login>Alice</login>
        <password>s3cr3t</password>
        <verify>bam</verify>
      </root>
  }

  val subclasses = new subclasses {
    def b = <root><name>B</name><foo>4</foo></root>
    def c = <root><name>C</name><bar>6</bar></root>
    def e = <root><name>E</name><eee>6</eee></root>
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
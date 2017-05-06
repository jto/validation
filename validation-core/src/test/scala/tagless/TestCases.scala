package jto.validation
package v3.tagless

trait TestCases[T] {
  trait base {
    def id: T
    def info: T
    def noInfo: T
    def jto: T
    def valid: T
    def invalid: T
    def smthTrue: T
    def smthFalse: T
    def emptyObj: T
  }

  trait int {
    def ok: T
    def foo: T
    def float: T
    def noOK: T
    def noFoo: T
    def nopOK: T
    def nopFoo: T
  }

  trait boolean {
    def ok: T
    def foo: T
  }

  trait string {
    def foo: T
    def foos: T
    def _42: T
    def onFoo: T
  }

  trait option {
    def nNull: T
    def fooBar: T
    def nBar: T
    def none: T
  }

  trait seq {
    def foos: T
    def fooBars: T
    def foofoobars: T
    def ns: T
    def ints: T
    def paf: T
    def mixed: T
  }

  trait map {
    def foobar: T
    def ints: T
    def mixed: T
  }

  trait password {
    val ok: T
    val empty: T
    val err: T
  }

  trait subclasses {
    val b: T
    val c: T
    val e: T
  }

  trait rec {
    val bobAndFriends: T
    val bobAndFriend: T
  }

  val base: base
  val int: int
  val boolean: boolean
  val string: string
  val option: option
  val seq: seq
  val map: map
  val password: password
  val subclasses: subclasses
  val rec: rec
}
import shapeless._
import scala.collection.generic.CanBuildFrom
import scala.util.Random

/** Type class for generating Arbitrary values of a type T */
trait Arbitrary[T] {
  def value: T
}

object Arbitrary {
  def apply[T](a: => T): Arbitrary[T] = new Arbitrary[T] { def value = a }
  def value[T](implicit a: Arbitrary[T]): T = a.value

  implicit val arbitraryInt = Arbitrary(Random.nextInt)
  implicit val arbitraryLong = Arbitrary(Random.nextLong)
  implicit val arbitraryDouble = Arbitrary(Random.nextDouble)
  implicit val arbitraryBoolean = Arbitrary(Random.nextBoolean)
  implicit val arbitraryString = Arbitrary(Random.alphanumeric.take(Random.nextInt(20)).mkString(""))

  implicit def arbitraryScalaCollection[Coll[_], T]
    (implicit
      cbf: CanBuildFrom[Nothing, T, Coll[T]],
      a: Arbitrary[T]
    ): Arbitrary[Coll[T]] =
      Arbitrary {
        val builder = cbf()
        while(Random.nextBoolean) builder += a.value
        builder.result()
      }

  implicit def arbitraryMap[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Map[A, B]] =
    Arbitrary(Arbitrary.value[List[(A, B)]].toMap)

  implicit def arbitraryCoproductBaseCase[H](implicit h: Arbitrary[H]): Arbitrary[H :+: CNil] =
    Arbitrary(Inl(h.value))

  implicit def arbitraryCoproductInductionStep[H, T <: Coproduct]
    (implicit
      h: Arbitrary[H],
      t: Arbitrary[T]
    ): Arbitrary[H :+: T] =
      Arbitrary((if(Random.nextBoolean) Inl(h.value) else Inr(t.value)))

  implicit val arbitraryHNil: Arbitrary[HNil] =
    Arbitrary(HNil)

  implicit def arbitraryProductInductionStep[H, T <: HList]
    (implicit
      h: Arbitrary[H],
      t: Arbitrary[T]
    ): Arbitrary[H :: T] =
      Arbitrary(h.value :: t.value)

  implicit def arbitraryGeneric[T, R]
    (implicit
      gen: Generic.Aux[T, R],
      arb: Lazy[Arbitrary[R]]
    ): Arbitrary[T] =
      Arbitrary(gen.from(arb.value.value))
}

import shapeless._

package object canmean {

  // The typeclass definition
  trait CanMean[T] {
    def combine(a: T, b: T): T
    def divideInt(a: T, b: Int): T
  }

  // The companion object of the typeclass, which will hold instances and instance derivations
  object CanMean extends CanMean1 {

    // apply[T] is a convenience method
    // CanMean[T] looks prettier than using implicitly[CanMean[T]]
    def apply[T : CanMean] = implicitly[CanMean[T]]


    // instance definition for ints
    implicit val intCanMean: CanMean[Int] = new CanMean[Int] {
      def combine(a: Int, b: Int) = a + b
      def divideInt(a: Int, i: Int) = a / i
    }
  }

  // The companion object inherits this
  // This one hold some derivations from existing Scala typeclasses
  trait CanMean1 extends GenericCanMean {
    implicit def fractionalCanMean[T](implicit frac: Fractional[T]) = new CanMean[T] {
      def combine(a: T, b: T) = frac.plus(a, b)
      def divideInt(a: T, i: Int) = frac.div(a, frac.fromInt(i))
    }

    implicit def integralCanMean[T](implicit integral: Integral[T]) = new CanMean[T] {
      def combine(a: T, b: T) = integral.plus(a, b)
      def divideInt(a: T, i: Int) = integral.quot(a, integral.fromInt(i))
    }
  }

  // The companion object inherits this (via CanMean1)
  // this holds derivations that allow any product type to get a CanMean instance
  // as long as all its fields have an instance
  trait GenericCanMean {

    // base case for recursive derivation
    implicit val hnilCanMean : CanMean[HNil] = new CanMean[HNil] {
      def combine(a: HNil, b: HNil) = HNil
      def divideInt(a: HNil, i: Int) = HNil
    }

    // recursive case for recursive derivation
    implicit def hconsCanMean[H : CanMean, T <: HList](implicit
      tailCanMean: CanMean[T]
    ) : CanMean[H :: T] = new CanMean[H :: T] {

      def combine(a: H :: T, b: H :: T) =
        CanMean[H].combine(a.head, b.head) :: tailCanMean.combine(a.tail, b.tail)

      def divideInt(a: H :: T, i: Int) =
        CanMean[H].divideInt(a.head, i) :: tailCanMean.divideInt(a.tail, i)
    }

    // derivation for product types - uses Generic to do the derivation in HList land
    implicit def genericCanMean[T <: Product, L <: HList](implicit
      generic: Generic.Aux[T, L],
      canMeanL: CanMean[L]
    ) : CanMean[T] = new CanMean[T] {
      def combine(a: T, b: T) =
        generic.from(canMeanL.combine(generic.to(a), generic.to(b)))

      def divideInt(a: T, i: Int) =
        generic.from(canMeanL.divideInt(generic.to(a), i))
    }
  }


  // the definition of the mean function, which uses the typeclass
  def mean[T : CanMean](nums: Seq[T]) =
    CanMean[T].divideInt(
      nums.reduceLeft(CanMean[T].combine),
      nums.length)

  // an implicit class for adding a ".mean" operation to anything that has a CanMean
  implicit class MeanOps[T : CanMean](val nums: Seq[T]) {
    def mean = canmean.mean(nums)
  }

}
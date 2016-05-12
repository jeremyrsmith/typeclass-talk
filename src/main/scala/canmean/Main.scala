package canmean

object Main extends App {

  val ints = Seq(1, 2, 3, 4, 5)
  println(ints.mean)

  val doubles = Seq(1.1, 2.2, 3.3, 4.4, 5.5)
  println(doubles.mean)

  case class Complex[T](real: T, imag: T)

  val complexInts =
    Seq(Complex(1, 1), Complex(2, 2), Complex(3, 3), Complex(4, 4), Complex(5, 5))
  println(complexInts.mean)

  val complexDoubles =
    Seq(Complex(1.1, 1.1), Complex(2.2, 2.2), Complex(3.3, 3.3), Complex(4.4, 4.4), Complex(5.5, 5.5))
  println(complexDoubles.mean)

  println(Seq((1,2), (1,5), (2,4)).mean)
  println(Seq((1.1, 2, 4), (3.3, 5, 1)).mean)

}
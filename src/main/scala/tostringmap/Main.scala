package tostringmap

object Main extends App {

  case class Foo(foo: Int, bar: Boolean, baz: Double)
  val a = Foo(10, true, 22.2)
  println(ToStringMap[Foo].apply(a))

}


class: center, middle

# Scala Typeclasses
## Now With Bonus Shapeless

---

class: bigger-content

# Agenda

1. Introduction
2. The problem
2. Review: Scala's type system
3. Review: Implicits
4. Typeclasses
5. Shapeless
6. Questions, plugs, workshopping, etc.

---

class: introduction

# Introduction .light[/ Welcome to Acorns HQ!]


.me-heading[Jeremy Smith - .light[Sr. Scala Engineer at Acorns]]
.fa-github-square[ jeremyrsmith]
.fa-linkedin-square[ linkedin.com/in/duckwizard]
.fa-external-link-square[ gentleman-and-a-scala.net]

---

class: center,middle
# We're Hiring
<p id="logo-holder"><img src="acorns.svg" alt="Acorns Logo" id="logo" /></p>
## Scala Engineer
## Data Pipeline Engineer
### http://acorns.com/careers

---

class: bigger-content

# The Problem

Let's say we wrote a simple function:

```scala
// Finds the mean of the given numbers
def mean(numbers: Seq[Int]) = a.reduceLeft(_ + _) / a.length
```

(ignore the obvious issues for sake of example)

---

class: bigger-content

# The Problem

But now we want to use it for `Double`s as well...

```scala
// Finds the mean of the given numbers
def mean(nums: Seq[Int]) = nums.reduceLeft(_ + _) / nums.length
def mean(nums: Seq[Double]) = nums.reduceLeft(_ + _) / nums.length
```

---

class: bigger-content

# The Problem

And what about `Float`? What about `BigDecimal`, `BigInteger`, `Short`, etc?

--

What if someone wants to use `mean` on a number type we didn't think of?

--

We want to write this function *generically* over all types that meet a certain criteria (add them together, divide
by `Int`)

---

class: bigger-content

# OOP Solution #1 - .light[Traits]


```scala
trait Number {
  def +(other: Number) : Number
  def /(dividend: Int) : Number 
}

def mean(nums: Seq[Number]) = nums.reduceLeft(_ + _) / nums.length
```

--

Problem solved!

--

As long as all numbers extend our `Number` trait...

--

Which they don't.

---

class: big-content

# OOP Solution #2 - .light[Wrap Everything]

```scala
trait Meanie[T] {
  def +(other: T) : Meanie[T]
  def /(i: Int) : T
}

class IntMeanie(value: Int) extends Meanie[Int] {
  def +(other: Int) = Meanie(value + other)
  def /(i: Int) = value / i
}

def mean[T](nums: Seq[Meanie[T]]) = nums.reduceLeft(_ + _) / nums.length
```

--

It's getting closer, but now everybody has to wrap all their values in some particular subclass
of `Meanie`.

```scala
mean(Seq(new IntMeanie(10), new IntMeanie(20), new IntMeanie(30))
```

It's kind of ugly and weird.

---

class: bigger-content

# Typeclasses

Define a *behavior* independently:

```scala
trait CanMean[T] {
  def combine(other: T) : T
  def divideInt(i: Int) : T
}

def mean[T : CanMean](nums: Seq[T]) = ???
```

`CanMean` is a typeclass. The `T : CanMean` is a *context bound*, which is a bound for a typeclass.

---

class: bigger-content

# Type vs. Type Constructor

```scala
class Foo[T]
```

`Foo[String]` is a type.

--

`Foo` is a *type constructor*.

---

class: big-content

# Type constructors

A value constructor constructs a value.

```scala
// constructor
class Foo(bar: Int, baz: String)

//value
new Foo(47, "Hello!")
```

--

A *type constructor* constructs a type

```scala
// type constructor
class Foo[T]

// type
type StringFoo = Foo[String]

def doThing(foo: Foo[String]) //foo has a type
```

---

class: bigger-content

# Type constructors

```scala
class    Foo            [T]
//       ┗┳┛            ┗┳┛
//Type Constructor    Shape: [_]

class    Foo            [T, U]
//       ┗┳┛            ┗━┳━━┛
//Type Constructor    Shape: [_,_]
```

--

"Shape" refers to the *arity* of the type constructor - how many type arguments it needs to construct a type.

The arguments themselves might have a shape - that's a *higher-kinded* type constructor.

---

class: center, middle

# Questions so far?

---

# Review - .light[Implicits]

--

* Defined using the 'implicit' keyword

--

* An implicit *value* has a type:
  ```scala
  implicit val foo : Int = 47
  ```
  
--

* An implicit *function* can construct a type:
  ```scala
  implicit def foo[T] : Foo[T] = ???
  ```
  (We'll talk about what that means)

--

* An implicit of type `T` is "the value" of type `T` in a particular scope.

---

class: bigger-content

# What are implicits good for?

--

* Many people use them as a dependency injection mechanism

--

  * (Not their intended use, but that's OK.)

--

* The Scala compiler uses them to enforce *type bounds*

---

# Review - .light[Type bounds]

## 
```scala
def foo[T <: Number](t: T) = ??? 
//can only be called on values that extend `Number`!

foo(22.5) //OK
foo(BigDecimal(22.5)) //OK
foo("blarg") //compile error
```

--

How did this work?

```scala
// this means the same thing!
def foo[T](t: T)(implicit tIsNumber: T <:< Number) = ???
```

--

* In this case, the *compiler* generated the necessary implicit.<br /> 
  (This is also known as "evidence" that `T` is a subclass of `Number`)

--

* `T <:< Number` can also be written as `<:<[T, Number]`. 

--

* `<:<` is actually a typeclass too!

---

class: center, middle

# Questions?

---

class: bigger-content

# Typeclasses

A typeclass is a type constructor which specifies some behavior over its type parameter(s):

```scala
trait CanMean[T] {
  def combine(a: T, b: T) : T
  def divideInt(a: T, i: Int) : T
}
```

---

class: bigger-content

# Typeclasses

Then we can write functions which require an *instance* of that typeclass to be available for a type argument:

```scala
def mean[T : CanMean](nums: Seq[T]) = ???
//same as:
def mean[T](nums: Seq[T])(implicit canMean: CanMean[T]) = ???
```

---

class: bigger-content

# Typeclasses

And we can use the functionality of the typeclass:

```scala
def mean[T : CanMean](nums: Seq[T]) = 
  CanMean[T].divideInt(
    nums.reduceLeft(CanMean[T].combine),
    nums.length)

//or

def mean[T](nums: Seq[T])(implicit canMean: CanMean[T]) = 
  canMean.divideInt(
    nums.reduceLeft(canMean.combine),
    nums.length)
```

---

class: bigger-content

# Typeclass Instances

* An *instance* is the (singular) definition of the typeclass' functionality
  for a given type.
* There is either one or zero instances of `CanMean[T]` for any given `T` in implicit scope.
* If exactly one instance is found, a call to `mean[T]` will compile.
* If zero instances, or more than one instance, are found, compilation will fail.

---

class: center

<br><br><br><br><br><br><br>
# Where do instances come from?

--

## From *implicit scope*.

---

class: big-content

# Implicit Scope

Priority 1: Any explicitly defined `implicit`, or explicitly imported `implicit`, which is in scope at the *call site*:

```scala
package object foo.bar.mean {
  implicit val intCanMean : CanMean[Int] = 
    new CanMean[Int] {
      def combine(a: Int, b: Int) = a + b
      def divideInt(a: Int, i: Int) = a / i
    }
  //intMeans is now available throughout package foo.bar.mean
}
```

```scala
import foo.bar.mean._
//intCanMean is now available in this file too
```

---

class: big-content

Priority 2: Companion object for the typeclass as well as the
type argument

```scala
trait CanMean[T] {
  //...
}

object CanMean {
  implicit val intMeans : Means[Int] = ???
  // no imports required anywhere!
}

class Foo

object Foo {
  implicit val fooMeans: Means[Foo] = ???
  // also no imports required
}
```

There are a few other levels, but that's another show.

---

class: bigger-content

# So what does the user's code look like?

```scala
val someNumbers = Seq(1,2,3,4,5,6)
println(mean(sumNumbers))
```

Not bad.

--

It can be a little better.

---

class: bigger-content

# Ops

An implicit class can be used to add operations around typeclasses

```scala
implicit class MeanOps[T : CanMean](val nums: Seq[T]) {
  def mean = mean.mean(nums)
  //assuming the function we defined was in package `mean`
}
```

--

```scala
Seq(1, 2, 3, 4, 5, 6).mean
```

---

class: big-content

# The win

What happens when someone wants to use our function for a type that
we didn't think of? Or their own data type?

```scala
case class ComplexInt(real: Int, imag: Int)
object ComplexInt {
  implicit val canMean = new CanMean[ComplexInt] {
    def combine(a: ComplexInt, b: ComplexInt) = 
      ComplexInt(a.real + b.real, a.imag + b.imag)
    def divideInt(a: ComplexInt, i: Int) = 
      ComplexInt(a.real / i, a.imag / i)
  }
}

val numbers = Seq(ComplexInt(1,0), ComplexInt(2,1)...)
println(numbers.mean)
```

Neat.  It's open for extension.

---

class: bigger-content

# Derivation .light[(The bigger win)]

What about this?

```scala
case class Complex[T](real: T, imag: T)
```

--

```scala
object Complex {
  implicit def canMean[T] : CanMean[Complex[T]] = ???
  // an implicit def can specify a derivation
}
```

---

# Derivation

Usually a derivation will have its own context bounds:

```scala
object Complex {
  implicit def canMean[T : CanMean] : CanMean[Complex[T]] =
    new CanMean[Complex[T]] {
      def combine(a: Complex[T], b: Complex[T]) = 
        Complex(
          CanMean[T].combine(a.real, b.real),
          CanMean[T].combine(a.imag, b.imag))
      def divideInt(a: Complex[T], b: Int) = 
        Complex(
          CanMean[T].divideInt(a.real, b),
          CanMean[T].divideInt(a.imag, b))
    }
}
```

--

Now, for any `T` that has a `CanMean`, `Complex[T]` does too!

--

Come on.  That's pretty cool.

---

class: bigger-content

# Functionality

Our `CanMean` typeclass does two things -- combine and scale. These could just as easily be separate typeclasses, if
their individual functionality might be useful outside of calculating a mean.

```scala
trait Combine[T] {
  def combine(a: T, b: T) : T
}

trait DivideByInt[T] {
  def divideByInt(a: T, i: Int) : T
}

def mean[T : Combine : DivideByInt] = ...
```

---

class: center

<br><br><br><br><br><br><br>
# 🚨 FP Alert! 🚨
## `Combine` looks an awful lot like `Group`.

```scala
def mean[T : Group : DivideByInt] = ...
```

--

These things come up all the time!

---

class: center, middle

# Questions?

---

class: bigger-content

# Uh oh.

```scala
case class Vector2D[T : CanMean](x: T, y: T)
case class Vector3D[T : CanMean](x: T, y: T, z: T)
case class Vector4D[T : CanMean](x: T, y: T, z: T, α: T)
//...
case class Vector20D[T : CanMean](...)
```

--

We're in for a lot of boilerplate...

---

class: center,middle

# Shapeless
## .light[Scrap Your Boilerplate!]

---

class: bigger-content

# Shapeless

Shapeless is a Swiss Army knife of tools for typelevel programming.

--

We're going to focus on its tools for *arity abstraction* and *typeclass derivation*.

---

class: bigger-content

# Core Tools

1. Heterogenous List (`HList`)
2. `Generic`
3. Singleton types
4. *Proofs* based on the above

---

class: bigger-content

# .light[Shapeless - ] HList

A Scala `List[T]` can have any size, but only contains elements of type `T` (or substitutable things)

```scala
val items = List(10, "foo", false)
// items is of type List[Any] :(
```

--

A Scala Tuple (i.e. `(10, "Foo", false)`) can have heterogenous types, but is of a fixed size.

```scala
val items = (10, "foo", false)
// size is hard-coded to 3 :(
```

---

class: bigger-content

# .light[Shapeless - ] HList

A `List[T]` really looks like this:
```
(T,
  (T,
    (T,
      (T, Nil))))
```

--

What about:
```
(A,
  (B,
    (C,
      (D, Nil))))
```

---

# .light[Shapeless - ] HList

This way we can retain all of the type information without hard-coding the length!

--

`HList` brings its own cons (`::`) operator, which is also a type:

```scala
val a = 10 :: "foo" :: false :: HNil
//a is of type Int :: String :: Boolean :: HNil
//aka ::[Int, ::[String, ::[Boolean, HNil]]]
```

--

The type of the HList must be known at compile time in order to do anything useful with it.

--

That's where proofs come in.

---

class: bigger-content

# .light[Shapeless - ] Proofs

A *proof* in shapeless is how you write functions over `HList` types for which you don't know the
specific type in your function definition.

--

Shapeless provides a bevy of typeclasses which function as proof that you can do certain things with
an `HList` type.

--

```scala
def length[L <: HList, N <: Nat](l: L)(implicit
  len: Length.Aux[L, N]
  toInt: ToInt[N]
) : Int = toInt()

println(length(10 :: "foo" :: false :: HNil))
//prints "3"
```

---

# .light[Shapeless - ] Proofs

What are these things?

```scala
def length[L <: HList, N <: Nat](l: L)(implicit
  len: Length.Aux[L, N],
  toInt: ToInt[N]
) : Int = toInt()
```

* `L` is some `HList` type.
* `N` is a `Nat` type, which is a type-level natural number
  * When we say "type-level", we mean it's a type at compile time (rather than an `Int` at runtime)
* `Length.Aux[L, N]` proves that the `HList` of type `L` has length of type `N`
* `ToInt[N]` proves that `N` can be given as an `Int`, and provides access to its `Int`.
* The `Aux` pattern is used frequently in shapeless, to ascribe a dependent type of a proof argument to an inferred
  type in the type argument list.

---

# What?

```scala
trait Length[L <: HList] {
  type Out <: Nat
}

object Length {
  type Aux[L <: HList, Out0 <: Nat] = Length[L] { type Out = Out0 } 
}
```

--

* `implicit len: Length[L]` says "there must be a `Length` for `L`".  But it could have any `Out`.
  * `Out` is a *dependent type*, in that it should be determined by the type arguments to `Length`
* `implicit len: Length.Aux[L, N]` says "there must be a `Length` for `L`, whose `Out` is `N`".
* This lets you put `N` in the type argument list of your function, and use it in subsequent proof steps.
* Or, it "links" `L` to `N`.


---

class: big-content

Notice that there is an `L` in the argument list.
 
```scala
def length[L <: HList, N <: Nat](l: L)(implicit
  len: Length.Aux[L, N], 
  toInt: ToInt[N]
) : Int = toInt()
```

--

`N` gets "linked" to `L` via `Length.Aux`.

--

As long as every type parameter is present in the argument list, or can be linked
back to one that is via a chain of `Aux`, then the type arguments can be inferred
(So the caller of `length` doesn't have to specify any type arguments)

--

That's good, because that type argument list can get pretty crazy (and it can contain
types that the caller wouldn't be able to specify if they wanted to)

---

class: small-code

# Example

```scala
def cogroupWith[
    Args <: Product,
    ArgsList <: HList,
    AllInputPairTypes <: HList,
    InputKeyTypes <: HList,
    InputValueTypes <: HList,
    OutputIterableTypes <: HList,
    Out <: Product](partitioner: Partitioner)(p: Args)(implicit
  argsGen: Generic.Aux[Args, ArgsList],                                              
  comapped: Comapped.Aux[(RDD[(K,V)] :: ArgsList), RDD, AllInputPairTypes],          
  toTraversable: ToTraversable.Aux[(RDD[(K,V)] :: ArgsList), List, RDD[_ <: (K, _)]],
  unzip: Unzip.Aux[AllInputPairTypes, (InputKeyTypes, InputValueTypes)],             
  mapped: Mapped.Aux[InputValueTypes, Iterable, OutputIterableTypes],                
  fromTraversable: FromTraversable[OutputIterableTypes],                             
  outTupler: Tupler.Aux[OutputIterableTypes, Out],
  classTagK: ClassTag[K]            
): RDD[(K, Out)] = {              
  val args = argsGen.to(p)
  val seq = toTraversable.apply(rdd :: args)
  val cogrouped = new CoGroupedRDD[K](seq, partitioner)
  cogrouped.flatMap {
    case (key, iterable) => fromTraversable(iterable) map {
      out => key -> outTupler.apply(out)
    }
  }
}
```

---

class: bigger-content

# .light[Shapeless -] Generic

Typically, you won't be directly using `HList`.

--

Rather, you'll use it behind the scenes to abstract over other types.

--

That's where `Generic` comes in.

```scala
type MyTuple = (Int, String, Boolean)
println(Generic[MyTuple].to((10, "foo", false)))
//result: 10 :: "foo" :: false :: HNil
```

---

# .light[Shapeless -] Generic

`Generic` is a "gateway proof".  It gets you from a `Product` type
(like a tuple) to an `HList` and back.

--

It lets you operate on `HList` rather than the product type, which lets you abstract the arity away.

--

```scala
implicit def productCanMean[T <: Product, L <: HList](implicit
  gen: Generic.Aux[T, L],
  ???
) : CanMean[T] = ???
```

--

* Using `Generic`, we can get from any product type to its `HList` representation and back.
* If we can derive `CanMean` for any `HList` type, then we can derive `CanMean` for the product type.

---

class: bigger-content

# .light[Shapeless -] Typeclasses over `HList`

Typeclasses over `HList` are defined using recursion.  Here's how to define your own typeclass "proof":

--

First, define an instance for `HNil` (the empty `HList`):

```scala
implicit val hnilCanMean : CanMean[HNil] = new CanMean[HNil] {
  def combine(a: HNil, b: HNil) = HNil
  def divideInt(a: HNil, i: Int) = HNil
}
```

--

This is the base case for the recursive derivation.

---

class: bigger-content

Then, define the recursive derivation:

```scala
implicit def hconsCanMean[H : CanMean, T <: HList](implicit
  tailCanMean: CanMean[T]
) : CanMean[H :: T] = new CanMean[H :: T] {

  def combine(a: H :: T, b: H :: T) =
    CanMean[H].combine(a.head, b.head) :: 
      tailCanMean.combine(a.tail, b.tail)

  def divideInt(a: H :: T, i: Int) =
    CanMean[H].divideInt(a.head, i) :: 
      tailCanMean.divideInt(a.tail, i)
}
```

---

Now we can derive `CanMean` for any `HList`, of any arity, as long as all of its constituent types themselves
have an instance of `CanMean`.

And with `Generic`, we can bridge the gap back to product types:

```scala
implicit def genericCanMean[T <: Product, L <: HList](implicit
  generic: Generic.Aux[T, L],
  canMeanL: CanMean[L]
) : CanMean[T] = new CanMean[T] {
  def combine(a: T, b: T) =
    generic.from(canMeanL.combine(generic.to(a), generic.to(b)))

  def divideInt(a: T, i: Int) =
    generic.from(canMeanL.divideInt(generic.to(a), i))
}
```

Which gives us this:

```scala
Seq((1,2), (1,5), (2,4)).mean
//(1,3)

Seq((1.1, 2, 4), (3.3, 5, 1)).mean
//(2.2,3,2)
```

---

It even gives us this:

```scala

case class Vector3D[T](a: T, b: T, c: T)

Seq(
  Vector3D(1, 2, 3),
  Vector3D(3, 4, 5),
  Vector3D(4, 5, 6)).mean
```

--

For any case class, of any arity.  With no boilerplate.

--

`Complex[T]` doesn't need its own derivation anymore.

--

It doesn't even need a context bound to `CanMean` on its `T`.

--

It doesn't need to know anything about `CanMean`.  Nobody needs to know about it.  The user just needs to know that
they can call `.mean` on a sequence of just about anything that makes sense.

---

# Aside

Remember how we said `CanMean` could be seen as `Group` with another ability to divide by an integer?  Let's look at
`Group` for a second.

--

`Group[T]` just means that you can combine `T` with another `T` to get a `T`
* `1 + 2 = 3`, 
* `"foo" + "bar" = "foobar"` (it doesn't have to be commutative)
 
--

```scala
trait Group[T] {
  def combine(a: T, b: T): T
}

implicit class GroupOps[T : Group](val t: T) {
  def combine(b: T) = Group[T].combine(t, b)
}
```

---

class: bigger-content

Using the same technique, we could derive `Group` for any product type.  Which would be really neat:

```scala
case class MyData(a: Int, b: Double, c: String)

MyData(1, 2.2, "foo") combine MyData(2, 3.3, "bar")
//MyData(3, 5.5, "foobar")
```

--

MyData didn't need to know anything about `combine`.

---

class: bigger-content

It also works for arbitrarily nested product types!

```scala
case class Inner(foo: Int, bar: String)
case class Outer(a: Inner, b: Inner)

val a = Outer(Inner(2, "foo"), Inner(100, "hi"))
val b = Outer(Inner(6, "bar"), Inner(50, "hello"))

a combine b
//Outer(Inner(8, "foobar"), Inner(150, "hihello"))
```

---

class: center, middle

# Questions?

---

class: center, middle

# One more thing?

---

class: bigger-content

# LabelledGeneric

What if we care about the field names of a case class?

```scala
case class Foo(bar: String, baz: Int)
type KindaLikeFoo = (String, Int)
```

--

`LabelledGeneric` lets you use those as well.  It's driven by *singleton types*.

---

# Singleton Types

What's the type of `"foo"`?

--

* `"foo"` is an `Any`

* `"foo"` is a `String`

--

* `"foo"` is also of type `"foo"`.

--

* `"foo"` is the singleton type of `"foo"`.

---

class: bigger-content

# Singleton Types

To use the type of a singleton literal, you use `Witness`:

```
def foo[S <: String](s: S)(implicit witness: Witness.Aux[S]) =
  witness.value //you can get the value-level value out
```

`Witness` proves that `S` is a singleton `String` literal.

---

# Tagged types

Shapeless uses an interesting trick:

```scala
type FieldType[K, +V] = V with KeyTag[K, V]
trait KeyTag[K, +V]
```

A `FieldType[K, V]` can be substituted for a plain `V`, but it's also tagged with `K` at the type level.

--

If you have an `HList` of `FieldType`s, then each element of the `HList` can have a type-level label.

--

Combine that with singleton literals, and you can have string labeled `HList`s.

`LabelledGeneric` is just like `Generic`, except it gives you these labels in the `HList` representation.

---

```scala
trait ToStringMap[T] {
  def apply(t: T) : Map[String, String]
}

object ToStringMap {
  def apply[T : ToStringMap] = implicitly[ToStringMap[T]]
  
  implicit val hnilStringMap = new ToStringMap[HNil] {
    def apply(n: HNil) = Map.empty
  }

  implicit def hconsStringMap[K <: Symbol, V, T <: HList](implicit
    witness: Witness.Aux[K],
    tailStringMap: ToStringMap[T]
  ) : ToStringMap[FieldType[K, V] :: T] = new ToStringMap[FieldType[K, V] :: T] {
    def apply(n: FieldType[K, V] :: T) =
      Map(witness.value.name -> n.head.toString) ++ tailStringMap(n.tail)
  }

  implicit def productStringMap[P <: Product, L <: HList](implicit
    gen: LabelledGeneric.Aux[P, L],
    stringMapL: ToStringMap[L]
  ) : ToStringMap[P] = new ToStringMap[P] {
    def apply(n: P) =
      stringMapL(gen.to(n))
  }
}
```

---
class: bigger-content

```scala
case class Foo(foo: Int, bar: Boolean, baz: Double)

val a = Foo(10, true, 22.2)

ToStringMap[Foo].apply(a)

//Map("foo" -> "10", "bar" -> "true", "baz" -> "22.2")
```

---

# LabelledGeneric

This allows you to do powerful things which would normally use runtime reflection...

--

...**without** the runtime reflection.

--

* JSON encoders/decoders, statically derived at compile time (`circe-generic`)

--

* SQL mapping and query DSLs, statically derived at compile time (`sqltyped`)

--

* The possibilities are endless - **and it's all done at compile time**.

---

class: center,middle
<p id="logo-holder"><img src="acorns.svg" alt="Acorns Logo" id="logo" /></p>

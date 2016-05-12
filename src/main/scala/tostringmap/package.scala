import shapeless._
import shapeless.labelled._
package object tostringmap {
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
}

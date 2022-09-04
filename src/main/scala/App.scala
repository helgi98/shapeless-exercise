import cats.Monoid
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Align, Diff, Init, Intersection, IsHCons, Last, Mapper, Prepend}
import shapeless.ops.nat.ToInt
import shapeless.ops.tuple.Length
import shapeless.syntax.singleton.mkSingletonOps
import shapeless.{Inl, _}

import scala.util.chaining.scalaUtilChainingOps


trait CsvEncoder[A] {
  val width: Int

  def encode(value: A): List[String]
}

object CsvEncoder {

  def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] = encoder

  def pure[A](width: Int)(f: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    override val width: Int = width

    override def encode(value: A): List[String] = f(value)
  }


  implicit val booleanEncoder: CsvEncoder[Boolean] = CsvEncoder.pure(1)(x => List(if (x) "yes" else "no"))

  implicit val stringEncoder: CsvEncoder[String] = CsvEncoder.pure(1)(x => List(x))

  implicit val intEncoder: CsvEncoder[Int] = CsvEncoder.pure(1)(x => List(x.toString))

  implicit val doubleEncoder: CsvEncoder[Double] = CsvEncoder.pure(1)(x => List(x.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = CsvEncoder.pure(1)(x => Nil)

  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
    CsvEncoder.pure(1)(ls => hEncoder.value.encode(ls.head) ++ tEncoder.encode(ls.tail))

  implicit val cnilEncoder: CsvEncoder[CNil] = CsvEncoder.pure(0)(_ => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] =
    CsvEncoder.pure(1 + tEncoder.width) {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R], encoder: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    CsvEncoder.pure(1)(v => encoder.value.encode(gen.to(v)))
}

sealed trait JsonValue

case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue

case class JsonArray(items: List[JsonValue]) extends JsonValue

case class JsonString(value: String) extends JsonValue

case class JsonNumber(value: Double) extends JsonValue

case class JsonBoolean(value: Boolean) extends JsonValue

case object JsonNull extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}


object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](f: A => JsonValue): JsonEncoder[A] = f(_)

  def createNullEncoder[A](f: A => JsonValue): JsonEncoder[A] = a => if (a == null) JsonNull else f(a)

  implicit val stringEncoder: JsonEncoder[String] = createNullEncoder(JsonString)

  implicit val booleanEncoder: JsonEncoder[Boolean] = createNullEncoder(JsonBoolean)

  implicit val doubleEncoder: JsonEncoder[Double] = createNullEncoder(JsonNumber)

  implicit val floatEncoder: JsonEncoder[Float] = createNullEncoder(JsonNumber(_))

  implicit val longEncoder: JsonEncoder[Long] = createNullEncoder(JsonNumber(_))

  implicit val intEncoder: JsonEncoder[Int] = createNullEncoder(JsonNumber(_))

  implicit val byteEncoder: JsonEncoder[Byte] = createNullEncoder(JsonNumber(_))

  implicit val shortEncoder: JsonEncoder[Short] = createNullEncoder(JsonNumber(_))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] = createNullEncoder { ls =>
    JsonArray(ls.map(enc.encode))
  }

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createNullEncoder(_.fold[JsonValue](JsonNull)(enc.encode))

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] = fn(_)

  implicit val hnilEncoder: JsonEncoder[HNil] = createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                              hEncoder: Lazy[JsonEncoder[H]],
                                                              tEncoder: JsonObjectEncoder[T]): JsonObjectEncoder[FieldType[K, H] :: T] =
    createObjectEncoder {
      case head :: tail =>
        JsonObject((witness.value.name, hEncoder.value.encode(head)) :: tEncoder.encode(tail).fields)
    }

  implicit def genericObjectEncoder[A, H](implicit generic: LabelledGeneric.Aux[A, H],
                                          hEncoder: JsonObjectEncoder[H]): JsonObjectEncoder[A] =
    createObjectEncoder { obj =>
      hEncoder.encode(generic.to(obj))
    }

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] = createObjectEncoder {
    throw new UnsupportedOperationException("Won't happen")
  }

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit witness: Witness.Aux[K],
                                                                      hEncoder: Lazy[JsonEncoder[H]],
                                                                      tEncoder: JsonObjectEncoder[T]):
  JsonEncoder[FieldType[K, H] :+: T] = createObjectEncoder {
    case Inl(h) => JsonObject(List(witness.value.name -> hEncoder.value.encode(h)))
    case Inr(t) => tEncoder.encode(t)
  }
}

trait Last1[L <: HList] {
  type Out

  def apply(in: L): Out
}

object Last1 {
  type Aux[L <: HList, O] = Last1[L] {type Out = O}

  def apply[A <: HList](implicit last: Last1[A]): Last1[A] = last

  implicit def pair[H]: Aux[H :: HNil, H] = new Last1[H :: HNil] {
    override type Out = H

    override def apply(in: H :: HNil): H = in.head
  }

  implicit def list[H, T <: HList]
  (implicit last: Last1[T]): Aux[H :: T, last.Out] = new Last1[H :: T] {
    override type Out = last.Out

    override def apply(in: H :: T): last.Out = last(in.tail)
  }
}

trait Penultimate[L] {
  type Out

  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] {type Out = O}

  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

  implicit def hlistPenultimate[L <: HList, M <: HList, O](implicit init: Init.Aux[L, M],
                                                           last: Last.Aux[M, O]):
  Penultimate.Aux[L, O] = new Penultimate[L] {
    type Out = O

    def apply(l: L): O = last(init(l))
  }
}

trait Migration[A, B] {
  def apply(a: A): B
}

object Migration {
  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  implicit def genericMigration[
    A, B,
    ARepr <: HList, BRepr <: HList,
    Unaligned <: HList
  ](implicit aGen: LabelledGeneric.Aux[A, ARepr],
    bGen: LabelledGeneric.Aux[B, BRepr],
    inter: Intersection.Aux[ARepr, BRepr, Unaligned],
    align: Align[Unaligned, BRepr]
   ): Migration[A, B] = new Migration[A, B] {
    def apply(a: A): B =
      bGen.from(align.apply(inter.apply(aGen.to(a))))
  }

  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty: A = zero

      def combine(x: A, y: A): A = add(x, y)
    }

  implicit val hnilMonoid = createMonoid(HNil)((_, _) => HNil)

  implicit def emptyHList[K <: Symbol, H, T <: HList](implicit hMonoid: Lazy[Monoid[H]],
                                                      tMonoid: Monoid[T]
                                                     ): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
      (x, y) =>
        field[K](hMonoid.value.combine(x.head, y.head)) ::
          tMonoid.combine(x.tail, y.tail)
    }

  implicit def genericMigration[
    A, B, ARepr <: HList, BRepr <: HList,
    Common <: HList, Added <: HList, Unaligned <: HList
  ](implicit aGen: LabelledGeneric.Aux[A, ARepr],
    bGen: LabelledGeneric.Aux[B, BRepr],
    inter: Intersection.Aux[ARepr, BRepr, Common],
    diff: Diff.Aux[BRepr, Common, Added],
    monoid: Monoid[Added],
    prepend: Prepend.Aux[Added, Common, Unaligned],
    align: Align[Unaligned, BRepr]
   ): Migration[A, B] =
    (a: A) => bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))
}

object myPoly extends Poly1 {
  implicit def caseInt: myPoly.Case.Aux[Int, String] = at(i => i.toString)

  implicit def caseString: myPoly.Case.Aux[String, String] = at(s => s)

  implicit def caseBoolean: myPoly.Case.Aux[Boolean, String] = at(b => if (b) "yes" else "no")
}

trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

object ProductMapper {
  implicit def genericMapper[A, B, ARepr <: HList, BRepr <: HList, P <: Poly]
  (implicit aGen: LabelledGeneric.Aux[A, ARepr],
   bGen: LabelledGeneric.Aux[B, BRepr],
   mapper: Mapper.Aux[P, ARepr, BRepr]
  ): ProductMapper[A, B, P] = a => bGen.from(mapper(aGen.to(a)))

  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)
                          (implicit pm: ProductMapper[A, B, P]): B =
        pm.apply(a)
    }

    def mapTo[B]: Builder[B] = new Builder[B]
  }
}

trait SizeOf[A] {
  def value: Int
}

object SizeOf {
  def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

  def apply[A: SizeOf]: Int = sizeOf[A]

  def genericSizeOf[A, L <: HList, N <: Nat](implicit generic: Generic.Aux[A, L],
                                             length: Length.Aux[L, N],
                                             toInt: ToInt[N]): SizeOf[A] =
    new SizeOf[A] {
      def value: Int = toInt()
    }
}

trait Random[A] {
  def get: A
}

object Random {
  def random[A](implicit random: Random[A]): A = random.get

  def apply[A: Random]: A = random[A]

  def createRandom[A](f: () => A): Random[A] = new Random[A] {
    def get: A = f()
  }

  implicit val intRandom: Random[Int] = createRandom(scala.util.Random.nextInt)

  implicit val longRandom: Random[Long] = createRandom(scala.util.Random.nextLong)

  implicit val shortRandom: Random[Short] = createRandom(() => scala.util.Random.nextInt().toShort)

  implicit val byteRandom: Random[Byte] = createRandom(() => scala.util.Random.nextInt().toByte)

  implicit val booleanRandom: Random[Boolean] = createRandom(scala.util.Random.nextBoolean)

  implicit val floatRandom: Random[Float] = createRandom(scala.util.Random.nextFloat)

  implicit val doubleRandom: Random[Double] = createRandom(scala.util.Random.nextDouble)

  implicit val charRandom: Random[Char] = createRandom(scala.util.Random.nextPrintableChar)

  implicit val hnilRandom: Random[HNil] = createRandom(() => HNil)

  implicit def hlistRandom[K <: Symbol, H, T <: HList](implicit hRandom: Lazy[Random[H]],
                                                       tRandom: Random[T]
                                                      ): Random[FieldType[K, H] :: T] =
    createRandom(() => field[K](hRandom.value.get) :: tRandom.get)

  implicit def genericRandom[A, L <: HList](implicit gen: Generic.Aux[A, L],
                                            random: Random[L]): Random[A] =
    createRandom(() => gen.from(random.get))

  implicit val cnilRandom: Random[CNil] = createRandom(() => throw new Exception("No, not going to happen"))

  implicit def coproductRandom[A, B <: Coproduct, N <: Nat](implicit aRandom: Lazy[Random[A]],
                                                            bRandom: Random[B],
                                                            bLength: Length.Aux[B, N],
                                                            bLengthInt: ToInt[N]
                                                           ): Random[A :+: B] =
    createRandom(() => if (scala.util.Random.nextInt(bLengthInt() + 1) == 0) Inl(aRandom.value.get) else Inr(bRandom.get))
}

object App extends App {
  sealed trait Shape

  final case class Circle(radius: Double) extends Shape

  final case class Rectangle(width: Double, height: Double) extends Shape

  def area(shape: Shape): Double = shape match {
    case Circle(r) => math.Pi * r * r
    case Rectangle(w, h) => w * h
  }

  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]

  def area2(shape2: Shape2): Double = shape2 match {
    case Left((w, h)) => w * h
    case Right(r) => math.Pi * r * r
  }

  val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

  val productTail: Int :: Boolean :: HNil = product.tail

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreamGen: Generic.Aux[IceCream, String :: Int :: Boolean :: HNil] = Generic[IceCream]

  val iceCream = IceCream("Sundae", 1, false)

  val repr = iceCreamGen.to(iceCream)

  val iceCream2 = iceCreamGen.from(repr)

  case class Employee(name: String, number: Int, manager: Boolean)

  val employee = Generic[Employee].from(repr)

  case class Red()

  case class Green()

  case class Amber()

  type Light = Red :+: Green :+: Amber :+: CNil

  val red: Light = Inr(Inl(Green()))

  val genShape = Generic[Shape]

  println(genShape.to(Circle(1.0)))
  val rectRepr: Circle :+: Rectangle :+: CNil = genShape.to(Rectangle(3.0, 4.0))
  val rect: Shape = genShape.from(rectRepr)

  //  println(area(rect))


  //  implicit val employeeCsvEncoder: CsvEncoder[Employee] =
  //    (value: Employee) => List(value.name, value.number.toString, value.manager.toString)
  //
  //  implicit val iceCreamCsvEncoder: CsvEncoder[IceCream] = (value: IceCream) =>
  //    List(value.name, value.numCherries.toString, value.inCone.toString)

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
    values.map(encoder.encode).mkString("\n")

  //  implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] =
  //    (value: (A, B)) => aEncoder.encode(value._1) ++ bEncoder.encode(value._2)

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  println(writeCsv(employees zip iceCreams))

  val shapes: List[Shape] = List(Circle(1.0), Rectangle(3.0, 4.0))

  println(writeCsv(shapes))

  sealed trait Tree[A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  case class Bar(x: Int, s: String)

  case class Foo(bar: Bar)

  CsvEncoder[Tree[Int]]


  val last1 = Last[String :: Int :: HNil]

  def getRepr[A](value: A)(implicit gen: Generic[A]): gen.Repr =
    gen.to(value)

  trait Second[L <: HList] {
    type Out

    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] {type Out = O}

    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
      new Second[A :: B :: Rest] {
        type Out = B

        def apply(value: A :: B :: Rest): B = value.tail.head
      }

    def lastField[A, Repr <: HList](input: A)(
      implicit
      gen: Generic.Aux[A, Repr],
      last: Last1[Repr]
    ): last.Out = last.apply(gen.to(input))
  }

  def getWrappedValue[A, Repr <: HList, H](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, H, HNil]
  ): H = gen.to(in).head

  getWrappedValue(Circle(1.0))

  val xNarrowed = 42.narrow
  val xLiteral: 42 = 42

  trait Cherry

  val numOfCherries = 42.asInstanceOf[Int with Cherry]

  val someNumber = 123
  val cherries = "numOfCherries" ->> someNumber
  val cherriesTagged: FieldType[Cherry, Int] = field[Cherry](123)

  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K =
    witness.value

  println(getFieldName(cherries))

  val labelledGen = LabelledGeneric[IceCream].to(iceCream)

  val toInt = ToInt[nat._2]

}

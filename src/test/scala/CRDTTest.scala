import org.scalacheck.{Arbitrary, Gen}

class CRDTTest {}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import CRDTCounter.CRDT


object CounterSpecification extends Properties("Counter") {

  import CRDTCounter.Zaehler

  val genBoolArray = Gen.containerOf[Array, Boolean](true)
  val genIntArray = Gen.containerOf[Array, Int](Gen.choose(1, 100))

  def generate(): Zaehler = {
    val cla = new Zaehler()
    for (n <- 0 until cla.count.length) {
      cla.count(n) = Gen.choose(1, 100).sample.get
      cla.negCount(n) = Gen.choose(1, 100).sample.get
    }
    cla
  }

  def generateSizedInt(): Gen[Int] = {
    val gener = Gen.choose(0, 4)
    gener
  }

  def generateSizedIntWithParam(min: Int, max: Int): Gen[Int] = {
    val gener = Gen.choose(min, max)
    gener
  }

  def generate3(): Gen[(Zaehler, Gen[Int], Gen[Int])] = Gen.sized {
    val x = generate()
    val y = generateSizedInt()
    val z = generateSizedIntWithParam(1, 100)
    return (x, y, z)
  }

  def gen3Counter: Gen[(Zaehler, Zaehler, Zaehler)] = Gen.sized {
    val a = generate()
    val b = generate()
    val c = generate()
    return (a, b, c)
  }
  //negCount macht value kleiner
  property("negCount") = forAll(generate3) {
    case (x, p, i) =>
      val counter = x.value()
      /*val r = p.sample.get
      val s = i.sample.get
      System.out.println(r+ " "+ s)*/
      x.decrement(p.sample.get, i.sample.get)
      val newVal = x.value()
      (counter > newVal) && (counter != newVal)

  }

  property("update") = forAll(generate, generateSizedInt(), generateSizedIntWithParam(1, 100)) { (x: Zaehler, p: Int, i: Int) =>
    val counter = x.value()
    x.update(p, i)
    val newVal = x.value()
    //System.out.println(p+ " "+i)
    (counter < newVal)

  }
  val c = new CRDT
  val commutative = forAll(gen3Counter) {
    case (x, y, z) =>
      c.merge(x, y).value() == c.merge(y, x).value()
  }
  val assosiative = forAll(generate, generate, generate) { (x: Zaehler, y: Zaehler, z: Zaehler) =>
    c.merge(z, c.merge(x, y)).value() == c.merge(x, c.merge(y, z)).value()
  }
  val idempotent = forAll(generate, generate) { (x: Zaehler, y: Zaehler) =>
    c.merge(x, c.merge(x, y)).value() == c.merge(x, y).value()
  }
  property("assosiative") = assosiative
  property("idem") = idempotent
  property("commu") = commutative
  property("merge") = assosiative && idempotent && commutative

}

object Hello {
  def main(args: Array[String]) = {
    val test = CounterSpecification.generate()
    for (n <- test.count)
      println(n)
  }
}


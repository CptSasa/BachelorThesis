import org.scalacheck.{Arbitrary, Gen}

class CaseClassTest {}

  import org.scalacheck.Prop.{forAll, propBoolean}
  import org.scalacheck.Properties
  import org.scalacheck.Prop.forAll
  object CaseClassSpecification extends Properties("CaseClass") {

    def generateCounter(): Counter = {
      var count = Counter(new Array[Int](5))
      for (countElements <- 0 until (5)) {
        //repeat 3-5 times
        for (repeat <- 0 until (generateParamSizedInt(3, 5))) {
          count = count.add(count, countElements, generateSizedInt().sample.get)
        }
      }
      return count
    }

    def generateSizedInt(): Gen[Int] = {
      val gener = Gen.choose(0, 100)
      gener
    }

    def generateParamSizedInt(min: Int, max: Int): Int = {
      Gen.choose(min, max).sample.get
    }

    val assosiative = forAll(generateCounter, generateCounter, generateCounter) { (x: Counter, y: Counter, z: Counter) =>
      x.merge(z, x.merge(x, y)).value() == x.merge(x, x.merge(y, z)).value()
    }
    val idempotent = forAll(generateCounter, generateCounter) { (x: Counter, y: Counter) =>
      x.merge(x, x.merge(x, y)).value() == x.merge(x, y).value()
    }
    val commutative = forAll(generateCounter, generateCounter) { (x: Counter, y: Counter) =>
      x.merge(x, x.merge(x, y)).value() == x.merge(x, y).value()
    }
    property("assosiative") = assosiative
    property("idem") = idempotent
    property("commu") = commutative
    property("merge") = assosiative && idempotent && commutative
  }

object printCounter {
  def main(args: Array[String]) = {
    val test = CaseClassSpecification.generateCounter();
    for (n <- test.count)
      println(n)
  }
}

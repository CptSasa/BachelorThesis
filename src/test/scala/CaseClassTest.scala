import org.scalacheck.{Arbitrary, Gen}

class CaseClassTest {}

  import org.scalacheck.Prop.{forAll, propBoolean}
  import org.scalacheck.Properties
  import org.scalacheck.Prop.forAll
  object CaseClassSpecification extends Properties("CaseClass") {

    def generate(): Counter = {
      var count = Counter(Array[Int](5))
        val c = Gen.choose(0,4).sample.get
      val g = Gen.choose(2,5)
      for(countElements <- 0 until(count.count.length)) {
        for (repeat <- 0 until(5)) {
        count = count.add(count,c,Gen.choose(1,100).sample.get)
        }
      }
      return count
    }


}

package CRDTCounter

class Zaehler {

  var count = new Array[Int](5)
  var negCount = new Array[Int](5)

  def update(p: Int, i: Int): Unit = {
    count(p) = count(p) + i
  }

  def decrement(p: Int, i: Int): Unit = {
    negCount(p) = negCount(p) + i
  }

  def query(): Int = {
    var max = 0;
    for (n <- count) {
      if (n > max) {
        max = n
      }
    }
    return max
  }

  def value(): Int = {
    var counter = 0
    var negCounter = 0
    for (n <- count) {
      counter = counter + n
    }
    for (m <- negCount) {
      negCounter = negCounter + m
    }
    counter - negCounter
  }

}
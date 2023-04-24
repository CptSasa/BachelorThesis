case class Counter(count: Array[Int]) {

  val message = Counter(Array[Int](6))

  def add(c: Counter, p: Int, i: Int): Counter = {
    val tmp = c.copy()
    tmp.count(p) = tmp.count(p) + i
    return Counter(tmp.count)
  }

  def value(d: Counter): Int = {
    var sum = 0
    for (a <- d.count) {
      sum = sum + a
    }
    return sum
  }

  def merge(first: Counter, second: Counter): Counter = {
    val tmp = first.copy()
    for (a <- 0 until tmp.count.length) {
      if (tmp.count(a) < second.count(a)) {
        tmp.count(a) = second.count(a)
      }
    }
    return tmp
  }

}
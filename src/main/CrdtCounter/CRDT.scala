package CRDTCounter

class CRDT() {

  def merge(x: Zaehler, y: Zaehler): Zaehler = {
    var z = new Zaehler()
    for (n <- 0 until (x.count.length)) {
      if (x.count(n) >= y.count(n)) {
        z.count(n) = x.count(n)
      }
      else {
        z.count(n) = y.count(n)
      }
      if (x.negCount(n) >= y.negCount(n)) {
        z.negCount(n) = x.negCount(n)
      }
      else {
        z.negCount(n) = y.negCount(n)
      }
    }

    z
  }

  def compare(x: Zaehler, y: Zaehler): Boolean = {
    var greaterEquals = true;
    for (n <- x.count) {
      if (x.count(n) > y.count(n)) {
        greaterEquals = false;
      }
      if (x.negCount(n) > y.negCount(n)) {
        greaterEquals = false;
      }
    }
    greaterEquals
  }
}

import scala.io.StdIn.readLine

case class Rang(low: Long, high: Long) {
  require(low <= high, "invalid Rang")

  def contains(n: Long): Boolean = n >= low && n <= high

  def combine(o: Rang): Option[Rang] = {
    val hasOverlap =
      this.contains(o.low) || this.contains(o.high) || o.contains(low) || o.contains(high)

    if (!hasOverlap) None
    else {
      val l = if (low < o.low) low else o.low
      val h = if (high > o.high) high else o.high

      Some(Rang(l, h))
    }
  }

  def count: Long = high - low + 1
}

var rangs = List.empty[Rang]
var ings = List.empty[Long]

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map{ s =>
      if (s.contains("-")) {
        val lh = s.split("-")
        rangs = Rang(lh(0).toLong, lh(1).toLong) :: rangs
      }
      else if (s.isEmpty) ()
      else ings = s.toLong :: ings
    }
      .foreach { _ => () }

var hadChange = false
def combine(rs: List[Rang], show: Boolean = false): List[Rang] = {
  hadChange = false

  rs.sortBy(_.low)
  .foldLeft(List.empty[Rang]){
    case (Nil, rang) => List(rang)
    case ((h :: ts), rang) =>
      h.combine(rang).fold(rang :: h :: ts){a =>
        hadChange = true
        if (show) println(s"$h - $rang\n    $a")
        a :: ts
      }
  }
}

rangs = rangs.reverse
//ings = ings.reverse

val total = combine(rangs).map(_.count).sum

println(s"Total: $total")

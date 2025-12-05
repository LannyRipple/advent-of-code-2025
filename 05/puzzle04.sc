import scala.io.StdIn.readLine

case class Rang(low: Long, high: Long) {
  def contains(n: Long): Boolean = n >= low && n <= high
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

rangs = rangs.reverse
ings = ings.reverse

val total = ings.map(i => if (rangs.exists(_.contains(i))) 1 else 0).sum

println(s"Total: $total")

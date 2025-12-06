import scala.io.StdIn.readLine

case class ColMaj(xss: IndexedSeq[IndexedSeq[Long]]) {
  def apply(i: Int) = (j: Int) => xss(j)(i)
}

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map(_.split(" ").toVector.filterNot(_.isEmpty))
    .toVector

val numsRowMajor = input.init.map(_.map(_.toLong))
val nums = numsRowMajor.transpose

val ops: Vector[(Long, (Long,Long) => Long)] = input.last.map {
  case "*" => (1L, (a: Long, b: Long) => a * b)
  case _ => (0L, (a: Long, b: Long) => a + b)
}

val results =
  nums.zipWithIndex.map { case (ns, i) =>
    ns.foldLeft(ops(i)._1)(ops(i)._2)
  }

val total = results.sum

println(s"Total: $total")

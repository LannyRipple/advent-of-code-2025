import scala.io.StdIn.readLine

case class ColMaj(xss: IndexedSeq[IndexedSeq[Long]]) {
  def apply(i: Int) = (j: Int) => xss(j)(i)
}

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .toVector

val numbers = input.init
val operations = input.last
val lanes = operations.zipWithIndex.filterNot(_._1 == ' ').map(_._2 - 1).drop(1).toSet

val numsRowMajor =
  numbers.map{ str =>
    str.zipWithIndex.map{
      case (c, i) if c == ' ' && lanes(i) => ','
      case (c, _) => c
    }
    .mkString
    .split(",")
    .toVector
  }

val height = numsRowMajor.length
val width = numsRowMajor(0).length

val nums =
  numsRowMajor.transpose
  .map(
    _.map(_.toVector)
      .transpose
      .map(_.mkString.trim.toLong)
  )

val ops: Vector[(Long, (Long,Long) => Long)] =
  input.last
    .split(" ")
    .filterNot(_.isEmpty)
    .map {
      case "*" => (1L, (a: Long, b: Long) => a * b)
      case _ => (0L, (a: Long, b: Long) => a + b)
    }
    .toVector

val results =
  nums.zipWithIndex.map { case (ns, i) =>
    ns.foldLeft(ops(i)._1)(ops(i)._2)
  }

val total = results.sum

println(s"Total: $total")

import scala.io.StdIn.readLine

case class Note(len: Int, fst: (Int, Int), snd: (Int, Int))

val total =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map(_.toList.map(_.toInt - '0'.toInt))
    .map(xs => (xs.init, xs.last))
    .map{ case (xs, x) =>
      val max = xs.max
      val nextmax = ((xs.dropWhile(_ != max).tail) :+ x).max
      max * 10 + nextmax
    }
    .sum

println(s"Total: $total")

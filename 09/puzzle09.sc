import scala.io.StdIn.readLine
import scala.math.abs

case class Node(x: Long, y: Long)

case class Edge(a: Node, b: Node, area: Long)

object Edge {
  def area(a: Node, b: Node): Long = (abs(a.x - b.x) + 1) * (abs(a.y - b.y) + 1)

  def apply(a: Node, b: Node): Edge =
    Edge(a, b, area(a, b))
}

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map { s =>
      val coords = s.split(",").map(_.toLong)
      Node(coords(0), coords(1))
    }
    .toList

val iter =
  for {
    (x, i) <- input.iterator.zipWithIndex
    (y, j) <- input.iterator.zipWithIndex
    if i < j
  } yield Edge(x, y)

val area = iter.maxBy(_.area).area

println(s"Area: $area")

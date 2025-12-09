import scala.io.StdIn.readLine
import scala.math.Ordered.orderingToOrdered
import scala.collection.mutable.PriorityQueue

val PAIRS = 1000

implicit val nodeOrdering: Ordering[Node] = {
  val byx: Ordering[Node] = Ordering.by(_.x)
  byx.orElseBy(_.y).orElseBy(_.z)
}

implicit val nodePairOrdering: Ordering[Edge] = Ordering.by(_.soh)

case class Node(x: Long, y: Long, z: Long)

case class Edge(a: Node, b: Node, soh: Long)

object Edge {
  def sq(n: Long): Long = n*n

  def apply(a: Node, b: Node): Edge = {
    val soh = sq(a.x - b.x) + sq(a.y - b.y) + sq(a.z - b.z)
    if (a <= b) Edge(a, b, soh) else Edge(b, a, soh)
  }
}

case class Circuit(nodes: Set[Node]) {
  def contains(e: Edge): Boolean = nodes.contains(e.a) || nodes.contains(e.b)
  def add(e: Edge): Circuit = this.copy(nodes = nodes ++ Set(e.a, e.b))
  def combine(c: Circuit): Circuit = this.copy(nodes = nodes ++ c.nodes)
  def size: Int = nodes.size

  override def toString(): String = s"Circuit(${nodes.toList.sorted.mkString(", ")})"
}

object Circuit {
  val empty: Circuit = Circuit(Set.empty[Node])

  def apply(n: Node): Circuit = Circuit(Set(n))
  def apply(e: Edge): Circuit = Circuit.empty.add(e)

  def combine(a: Circuit, b: Circuit): Circuit = a.combine(b)
}

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map { s =>
      val coords = s.split(",").map(_.toLong)
      Node(coords(0), coords(1), coords(2))
    }
    .toList
    .sorted

val iter =
  for {
    (x, i) <- input.iterator.zipWithIndex
    (y, j) <- input.iterator.zipWithIndex
    if i < j
  } yield Edge(x, y)

val closest =
  iter.foldLeft(PriorityQueue[Edge]()){ case (z, np) =>
    z += np
    if (z.size > PAIRS) z.dequeue()
    z
  }

val circuits =
  closest.foldLeft(input.map(n => Circuit(n))){ case (z, e) =>
    val (has, not) = z.partition(_.contains(e))
    val joined = has.foldLeft(Circuit(e))(Circuit.combine(_, _))

    joined :: not
  }

val total = circuits.map(_.size).sorted.reverse.take(3).product

//println(circuits.mkString("\n"))

println(s"Total: $total")

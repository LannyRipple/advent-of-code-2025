import scala.io.StdIn.readLine
import scala.math.abs

implicit val nodeOrdering: Ordering[Node] = {
  val byy: Ordering[Node] = Ordering.by(_.y)
  byy.orElseBy(_.x)
}

case class Node(x: Long, y: Long)

case class Rect(a: Node, b: Node, area: Long)

object Rect {
  def apply(a: Node, b: Node): Rect =
    Rect(a, b, area(a, b))

  def area(a: Node, b: Node): Long = (abs(a.x - b.x) + 1) * (abs(a.y - b.y) + 1)

  def boundingBox(a: Node, b: Node): Rect = {
    if (a.y > b.y && a.x > b.x)
      Rect(Node(b.x, a.y), Node(a.x, b.y))
    else Rect(a, b)
  }
}

case class Edge(a: Node, b: Node)

case class Join(a: Edge, b: Edge)

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map { s =>
      val coords = s.split(",").map(_.toLong)
      Node(coords(0), coords(1))
    }
    .toVector

val edges =
  input.sliding(2)
    .map { case List(a, b) => Edge(a, b) } :+ Edge(input.head, input.last)

val boundary =
  edges.sliding(3)
    .map { case List(a, b) =>
      /*
       * We'll find the outside for the middle section.  For simplicity
       * we'll discuss motion "up".  The rules
       *
       *   $ a.x==b.x, a.y > b.y -> outside is > a.x
       *   $         , a.y < b.y -> outside is < a.x
       *   $ a.y==b.y, a.x > b.x -> outside is < a.y
       *   $         , a.x < b.x -> outside is > a.y
       *
       *   $ + is start point
       *   $ 0 is straight onward
       *   $ 1 a cw turn
       *   $ 2 a ccw turn
       *   $ `.` is outside
       *
       *  000  001  002  010  011  012  020  021  022
       *   |   _     _    .    .   |.          |
       *   |.   |.  |.   __    _    _    __   _    _
       *   |    |   |      |  | |    |  |.   |.   |.|
       *   +    +   +      +    +    +  +    +    +
       *
       *  100   101   102   110    111   112   120   121   122
       *   .     .    | .     _      _     _    |    _      _
       *  ___    __    __   .| +  .|  +  .| +   |.    |.   |.
       *     +  |  +     +   |      _    _       _+    _+   _+
       *
       *  200   201   202   210    211   212   220   221   222
       *          |   __     |     _       _   _     _     _
       *  ___   __   + .|    |.     |.    |.  +.|   +.|   +.|
       * + .   + .         +_     +_    +_      |      _   _
       *
       *  |                 +               ___+             +---
       *  |.  (a.x+1,a.y)   | (a.x-1,a.y)   (a.x,a.y-1)      (a.x,a.y+1)
       *  |   (b.x+1,b.y)   | (b.x-1,b.y)   (b.x,b.y-1)      (a.x,b.y+1)
       *  +                 |
       *
       *  _                    _                   _
       *   | (a.x+1,a.y)      |  (a.x+1,a.y)      |   (a.x+1,a.y+1)
       *   | (b.x+1,b.y-1)    |  (b.x+1,b.y+1)  +_    (b.x+1,b.y+1)
       *   +                  +
       */

    }
    .toVector

println(input.mkString("", "\n", "\n"))

val seen = input.toSet

def insideBB(x: Node, y: Node): Boolean =
  input.exists{n =>
    val bb = Rect.boundingBox(x, y)
    n.x <= bb.a.x && n.y > bb.a.y && n.y <= bb.b.y
  }

val iterx =
  for {
    (x, i) <- input.iterator.zipWithIndex
    (y, j) <- input.iterator.zipWithIndex
    if i < j && (x.x == y.x || x.y == y.y || seen(Node(y.x, x.y))) && !insideBB(x, y)
  } yield Rect(x, y)

val iter = iterx.toList
println(iter.mkString("", "\n", "\n"))

val area = iter.maxBy(_.area)

println(s"Area: $area")

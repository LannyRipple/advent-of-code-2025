import scala.io.StdIn.readLine

def combine(a: Map[Int, Long], b: Map[Int, Long]): Map[Int, Long] =
  b.foldLeft(a) { case (z, (n, c)) =>
    val newkv = n -> (c + z.getOrElse(n, 0L))
    z + newkv
  }

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map(_.zipWithIndex.filterNot(_._1 == '.').toList)
    .toList
    .filterNot(_.isEmpty)

val ray = input.head.head._2
val splitters = input.tail.map(_.map(_._2).toSet)

val total =
  splitters.foldLeft(Map(ray -> 1L)){ case (rays, splits) =>
    //println(s"rayss = $rays")

    val next =
      rays.map { (ray, count) =>
        val unsplitRays = Set(ray).diff(splits)
        val splitRays = Set(ray).intersect(splits)
        val newRays = splitRays.flatMap(n => Set(n-1, n+1))

        val r =
          (unsplitRays ++ newRays).map(n => n -> count).toMap
        //println(r)
        r
      }
      .foldLeft(Map.empty[Int, Long]){ case (z, m) => combine(z, m) }

    //println(next.map(_._2).sum)
    //println(s"next = $next")
    //println()

    next
  }
  .map(_._2).sum

println(s"Total: $total")

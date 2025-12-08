import scala.io.StdIn.readLine

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map(_.zipWithIndex.filterNot(_._1 == '.').toVector)
    .toVector
    .filterNot(_.isEmpty)

val rays = Set(input.head.head._2)
val splitters = input.tail.map(_.map(_._2).toSet)

val (total, _) =
  splitters.foldLeft(0 -> rays){ case ((count, rays), splits) =>
    val unsplitRays = rays.diff(splits)
    val splitRays = rays.intersect(splits)
    val newRays = splitRays.flatMap(n => Set(n-1, n+1))

    /*
    println(s"(($count, $rays), $splits")
    println(s"unsplitRays = $unsplitRays")
    println(s"splitRays = $splitRays")
    println(s"newRays = $newRays")
    println()
    */

    (count + splitRays.size) -> (unsplitRays ++ newRays)
  }

println(s"Total: $total")

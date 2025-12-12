import scala.io.StdIn.readLine

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)


val total = results.sum

println(s"Total: $total")

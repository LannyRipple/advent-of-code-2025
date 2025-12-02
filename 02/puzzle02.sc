import scala.io.StdIn.readLine

val ranges =
  readLine()
    .split(",")
    .toList
    .map { range =>
      val fl = range.split("-")
      fl(0).toLong to fl(1).toLong
    }

val total =
  ranges.flatMap { r =>
    r.flatMap { i =>
      val s = i.toString
      if (s.length % 2 != 0)
        None
      else {
        val len = s.length
        val half = s.length / 2
        val a = s.substring(0, half)
        val b = s.substring(half, len)
        if (a != b)
          None
        else
          Some(i)
      }
    }
  }
  .sum

println(s"Total: $total")

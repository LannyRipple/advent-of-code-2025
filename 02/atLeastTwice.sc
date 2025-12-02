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
      val len = s.length

      (1 until len).flatMap { span =>
        if (len % span != 0)
          None
        else {
          val t = s.substring(0, span)
          val isNotRepeat = s.grouped(span).exists(_ != t)
          if (isNotRepeat)
            None
          else
            Some(i)
        }
      }
    }
  }
  .distinct

//println(total.mkString(", "))
println(s"Total: ${total.sum}")

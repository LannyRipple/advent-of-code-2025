import scala.io.StdIn.readLine

def pin[A](as: Vector[A]): String = as.mkString(", ")

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map(_.toVector.map { c => if (c == '.') 9 else 0 })
    .toVector

val w = input(0).length
val h = input.length

val top = Vector.fill(w + 2)(9)
val istar = top +: input.map(vs => 9 +: vs :+ 9) :+ top
val board = istar.map(_.toArray).toArray

def update(): Unit =
  (1 to h).foreach { i =>
    (1 to w).foreach { j =>
      val c = board(i)(j)

      if (c < 9) {
        board(i-1)(j-1) += 1
        board(i-1)(j) += 1
        board(i-1)(j+1) += 1
        board(i)(j-1) += 1
        board(i)(j+1) += 1
        board(i+1)(j-1) += 1
        board(i+1)(j) += 1
        board(i+1)(j+1) += 1
      }
    }
  }

def count(): Int = {
  var total = 0

  (1 to h).foreach { i =>
   (1 to w).foreach { j =>
     val c = board(i)(j)

     if (c < 4) {
       total += 1
       board(i)(j) = 9
     }
     else if (c < 9)
       board(i)(j) = 0
     else
       board(i)(j) = 9
   }
  }

  total
}

def show(): Unit = {
  (1 to h).foreach { i =>
   (1 to w).foreach { j =>
     print(f"${board(i)(j)}%2d ")
   }
   println()
  }
  println()
}

def loop(): Int = {
  var total = 0
  var step = 0

  while (step != 0 || total == 0) {
    //show()
    update()
    //show()
    step = count()
    total += step
    println(s"step=$step total=$total")
  }

  total
}

val total = loop()

println(s"Total: $total")

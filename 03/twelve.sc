import scala.io.StdIn.readLine

case class Note(skippable: Int, xs: List[Int], zs: List[Int] = List.empty[Int]) {
  val isDone = skippable == 0
  val answer = zs.reverse ++ xs
}

def prune(note: Note)(step: Note => Either[Note,Note]): Note = {
  var s: Either[Note,Note] = Right(note)

  while (s.isRight) {
    s = step(s.right.get)
  }

  s.left.get
}

def hunt(note: Note): Note = {
  var haveDropped = false

  prune(note) { case n @ Note(skip, xs, zs) =>
    if (n.isDone)
      Left(Note(skip, zs.reverse ++ xs, Nil))
    else
      xs match {
        case h1 :: h2 :: rest if h1 < h2 =>
          haveDropped = false
          Right(Note(skip - 1, zs.reverse ++ (h2 :: rest), Nil))

        case h1 :: h2 :: rest =>
          haveDropped = false
          Right(Note(skip, h2 :: rest, h1 :: zs))

        case h :: Nil if !haveDropped =>
          haveDropped = false
          Right(Note(skip - 1, zs.reverse, Nil))

        case _ =>
          haveDropped = false
          Right(Note(skip, zs.reverse ++ xs, Nil))
      }
    }
}

def smash(answer: List[Int]): Long = {
  var factor = 1L

  answer.reverse
    .foldLeft(0L) { case (z, n) =>
      val a = n * factor
      factor = factor * 10
      z + a
    }
}

val nums =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map(_.toList.map(_.toInt - '0'.toInt))
    .map(xs => Note(xs.length - 12, xs))
    .map(hunt)
    .map(_.answer)
    .map(smash)
    .toList

println(nums.mkString("", "\n", "\n"))

val total = nums.sum

println(s"Total: $total")

import scala.io.StdIn.readLine
import scala.math.abs

case class Machine(goal: Set[Int], buttons: List[Set[Int]], jolts: List[Int])

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map { line =>
      val tokens = line.split(" ")
      val goalText = tokens(0)
      val joltsText = tokens.last
      val buttonTexts = tokens.tail.reverse.drop(1).toList

      val goal =
        goalText.replace("[","")
          .replace("]","")
          .zipWithIndex
          .foldLeft(Set.empty[Int]){ case (z, (c, i)) =>
            if (c == '#') z + i else z
          }

      val jolts =
        joltsText.replace("{", "")
          .replace("}", "")
          .split(",")
          .map(_.toInt)
          .toList

      val buttons =
        buttonTexts.map { text =>
          text.replace("(", "")
            .replace(")", "")
            .split(",")
            .map(_.toInt)
            .toSet
        }

      Machine(goal, buttons, jolts)
    }

println(input.mkString("", "\n", "\n"))

def buttonsPowerSet(buttons: List[Set[Int]]): Vector[List[Set[Int]]] =
  (1 to buttons.length).toVector
    .flatMap(buttons.combinations)
    .flatMap(_.permutations.toList)

def pushButton(lights: Set[Int], button: Set[Int]): Set[Int] = {
  val intersection = lights.intersect(button)
  (lights -- intersection) ++ (button -- intersection)
}

def pushButtons(buttons: List[Set[Int]]): Set[Int] =
  buttons.foldLeft(Set.empty[Int]){ case (z, b) =>
    pushButton(z, b)
  }

println("PowerSet\n")
/*
val minPushes =
  input.map { machine =>
    val Machine(goal, buttons, _) = machine

    val bigList = buttons ++ buttons ++ buttons
    val pset = buttonsPowerSet(bigList)

    println(pset)
    println()

    val minButtons =
      pset
        .map(buttons => (pushButtons(buttons) == goal, buttons))
        .filter(_._1)
        .map(_._2)
        .minBy(_.length)

    println(minButtons.mkString("", "\n", "\n"))
    minButtons.length
  }
*/

//val total = minPushes.sum

//println(s"Total: $total")
val xs = List(Set(0, 1), Set(0, 2), Set(2, 3), Set(2), Set(1, 3), Set(3))
val len = buttonsPowerSet(xs ++ xs).length
println(len)
println(xs)

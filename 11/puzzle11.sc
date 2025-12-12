import scala.io.StdIn.readLine

case class Pot(xs: List[String], seen: List[String]) {
  def isEmpty: Boolean = xs.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def token: String = if (seen.isEmpty) "*" else seen.head

  def step: Pot =
    if (isEmpty) this else
      Pot(xs.tail, xs.head :: seen)
    }

  override toString(): String = s"${seen.reverse.mkString(" ")} | ${xs.mkString(" ")}"
}

object Pot {
  def apply(xs: List[String]): Pot = Pot(xs, List.empty[String])
}

val input =
  Iterator.continually(readLine())
    .takeWhile(_ != null)
    .map { txt =>
      val arr = txt.split(":")
      arr(0) -> arr(1).trim
    }
    .map { case (k, vt) => k -> Pot(vt.split(" ").toList) }
    .toMap

var total = 0
var hasDAC = false
var hasFFT = false

//var stack = List(Pot(List("you")))
var stack = List(Pot(List("svr")))

def path = stack.map(_.token).mkString(" ")
var seen = Set.empty[String]

while (stack.nonEmpty) {
  var hs  = stack.head
  val tss = stack.tail

  var hstail = if (hs.isEmpty) List.empty[String] else hs.tail

  if (hs.nonEmpty)
    hs.head match {
      case "dac" =>
        hstail = "DAC" :: hs.tail
        hasDAC = true
      case "fft" =>
        hstail = "FFT" :: hs.tail
        hasFFT = true
      case "DAC" =>
        hasDAC = false
        hs = hs.tail
      case "FFT" =>
        hasFFT = false
        hs = hs.tail
      case _ => ()
    }

  if (hs.isEmpty) {
    stack = tss
    seen -= hs.seen.head
  }
  else {
    val in = hs.head
    val tok = input(hs.head)


    if (seen(in)) {
      println(s"Cycle on $in")
      println(s"PATH: $path")
      path.takeWhile(_ != in).foreach { t => println(s"$t: ${input(t)}") }
      println(s"$in: ${input(in)}")
      System.exit(1)
    }

    seen += in
    if (tok.head == "out") {
      println(s"PATH: ${("out" :: path)}")
    }
    else {
      println(s"PATH: $path")
    }


    if (tok.head != "out")
      stack = tok :: hstail :: tss
    else {
      //total += 1
      if (hasDAC && hasFFT) total += 1
      seen -= path.head
      path = path.tail

      stack =
        if (hstail.isEmpty) {
          seen -= path.head
          path = path.tail
          tss
        }
        else
          hstail :: tss
    }
  }
}

import scala.io.StdIn.readLine

case class Pot(xs: List[String], seen: List[String]) {
  def head: String = xs.head
  def isEmpty: Boolean = xs.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def token: String = if (seen.isEmpty) xs.head else seen.head

  def step(extra: String = null): Pot =
    if (isEmpty) this
    else if (extra != null)
      Pot(extra :: xs.tail, xs.head :: seen)
    else
      Pot(xs.tail, xs.head :: seen)

  override def toString(): String = {
    val s = s"($token)"

    if (seen.isEmpty)
      s"$s ${xs.tail.mkString(" ")}"
    else
      s"${(s :: seen.tail).reverse.mkString(" ")} ${xs.mkString(" ")}"
  }
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
var cache = Map("out" -> 0, "OUT" -> 1)

val start = if (args.isEmpty) "you" else args(0)
var stack = List(Pot(List(start)))
def path = stack.map(_.token).mkString(" ")
var seen = Set.empty[String]

while (stack.nonEmpty) {
  var hs  = stack.head
  val tss = stack.tail

  if (hs.nonEmpty)
    hs.head match {
      case "DAC" =>
        hasDAC = false
        hs = hs.copy(xs = hs.xs.tail)
      case "FFT" =>
        hasFFT = false
        hs = hs.copy(xs = hs.xs.tail)
      case _ => ()
    }

  if (hs.isEmpty || hs.token == "OUT") {
    stack = tss
    seen -= hs.token
    println(s"seen removed ${hs.token}")

    val instar = hs.token
    if (!cache.contains(instar)) {
      val tok = input(instar)
      val counts = tok.xs.map(t => cache.get(t))
      if (!counts.exists(_.isEmpty))
        cache = cache + (instar -> counts.flatten.sum)
    }
    //println(s"PATH: $path")
    //println(stack.mkString("", "\n", "\n"))
  }
  else {
    val in = hs.head

    if (cache.contains(in)) {
      if (args(0) != "svr")
        total += cache(in)
      else {
       if (hasDAC && hasFFT)
        total += cache(in)
      }

      stack = hs.step() :: tss
    }
    else {

      val hstail =
        in match {
          case "dac" =>
            hasDAC = true
            hs.step("DAC")
          case "fft" =>
            hasFFT = true
            hs.step("FFT")
          case _ =>
            val instar = hs.token
            if (!cache.contains(instar)) {
              val tok = input(instar)
              val counts = tok.xs.map(t => cache.get(t))
              if (!counts.exists(_.isEmpty))
                cache = cache + (instar -> counts.flatten.sum)
            }
            hs.step()
        }

      val tok = if (in == "fft") Pot(List("OUT")) else input(in)
      val counts = tok.xs.map(t => cache.get(t))

      seen -= hstail.token
      println(s"step removed ${hs.token}")

      if (!counts.exists(_.isEmpty)) {
        cache = cache + (in -> counts.flatten.sum)

        total += cache(in)
        /*
        if (args(0) != "svr")
          total += cache(in)
        else {
         if (hasDAC && hasFFT)
          total += cache(in)
        }
        */

        stack = tok :: hstail :: tss
      }
      else {
        if (seen(in)) {
          println(s"Cycle on $in")
          System.exit(1)
        }
        seen += in

        stack = tok :: hstail :: tss
/*
        if (tok.token == "out") {
          cache = cache + (hs.head -> (cache.getOrElse(hs.head, 0) + 1))

          if (args(0) != "svr")
            total += 1
          else {
           if (hasDAC && hasFFT)
            total += 1
          }
        }
*/

        //println(s"PATH: $path")
        println(stack.mkString("", "\n", "\n"))
        println(cache)
      }
    }
  }
}

println(s"you -> ${cache.get("you")}")
println(s"dac -> ${cache.get("dac")}")
println(s"Total: $total")

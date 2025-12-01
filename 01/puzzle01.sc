import scala.io.StdIn.readLine

var done = false
var num = 50
var combo = 0

def mod(n : Int, z: Int): Int = {
  var x = n + z
  while (x < 0) x += 100
  x %= 100
  x
}

while (!done) {
  val in = readLine()

  if (in == null)
    done = true
  else {
    val inStar = in.replace("R", "+").replace("L", "-")

    num = mod(num, inStar.toInt)
    println(s";; $num")

    if (num == 0) combo += 1
  }
}

println(s"Combo: $combo")

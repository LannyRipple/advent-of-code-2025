import scala.io.StdIn.readLine

var done = false
var num = 50
var combo = 0

def mod(n : Int, z: Int): Int = {
  var x = n + z
  println(s";; $n + $z = $x")

  var isZero = n == 0

  while (x < 0) {
    print(s";; $x + 100 = ")
    x += 100
    print(x)
    if (!isZero) {
      print(" ***")
      combo += 1
    }
    isZero = false
    println()
  }

  while (x > 100) {
    print(s";; $x - 100 = ")
    x -= 100
    print(x)
    combo += 1
    print(" ***")
    println()
  }

  x %= 100
  if (x == 0) println(";; ***")
  x
}

while (!done) {
  val in = readLine()

  if (in == null || in == ".")
    done = true
  else if (in.startsWith("R") || in.startsWith("L")) {
    val inStar = in.replace("R", "+").replace("L", "-")

    num = mod(num, inStar.toInt)

    if (num == 0) combo += 1
  }
}

println(s"Combo: $combo")

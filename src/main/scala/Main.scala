object Main {
  def test(x: Float, y: Float): Unit = {
    val _x = Float32.floatToFloat32(x)
    val _y = Float32.floatToFloat32(y)
    val _sum = _x + _y
    val sum = x + y
    val f32sum = Float32.floatToFloat32(sum)

    println(_sum)
    println(f32sum)
    println(s"Diff: ${f32sum.exponent - _sum.exponent}, ${f32sum.mantissa - _sum.mantissa}")

    if (sum == _sum.toFloat)
      print("  * \033[32mOk\033[0m: ")
    else
      print("  * \033[31mFailed\033[0m: ")

    println(s"$x + $y = $sum (${_sum.toFloat})")
  }

  def tests() {
    test(4f, 1f)
    test(2f, 1f)
    test(1f, 2f)
    test(1023f, 1.2f)
    test(8.3f, 0.0f)
    test(4f, -1f)
    test(-4f, 1f)
    test(-4f, -1f)
    test(-1f, -4f)
    println(Float32.float32ToFloat(Float32.floatToFloat32(2.0f).SQRT))
  }

  def main(args: Array[String]) {
    // tests()
    val n = 4
    val nf = Float32.floatToFloat32(n)

    var oldAverage: Float32 = Float32.floatToFloat32(0.0f)
    var average: Float32 = Float32.floatToFloat32(0.0f)
    var variance: Float32 = Float32.floatToFloat32(0.0f)

    for (i <- 1 to n)
      oldAverage = oldAverage + Float32.floatToFloat32(Console.readFloat)

    oldAverage = oldAverage / nf

    while (true) {
      for (i <- 1 to n) {
        val value = Float32.floatToFloat32(Console.readFloat)
        val tmp = value - oldAverage

        average = average + value
        variance = variance + tmp * tmp
      }

      average = average / nf
      variance = variance / nf
      variance = variance.SQRT

      println(s"Average: ${Float32.float32ToFloat(average)}")
      println(s"Variance: ${Float32.float32ToFloat(variance)}")

      // Process and act

      oldAverage = average
    }
  }
}

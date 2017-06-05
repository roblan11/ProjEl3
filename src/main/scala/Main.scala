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

    println(s"Enter the first $n samples")

    /* ADD $n, 4, 0 */
    /* :start_first_loop
     * CMPEQ $n, 0
     * JMP :end_first_loop
     * READ $val
     * ADDF $oldAverage, $oldAverage, $val
     * SUB $n, $n, 1
     * JMP :start_first_loop
     * :end_first_loop
     */
    for (i <- 1 to n)
      oldAverage = oldAverage + Float32.floatToFloat32(Console.readFloat)

    /* DIVF $oldAverage, $oldAverage, 4 */
    oldAverage = oldAverage / nf

    println(s"Average: ${Float32.float32ToFloat(oldAverage)}")

    /* :main_loop */
    while (true) {
      /* ADD $n, 4, 0
       * ADD $average, 0, 0
       * ADD $variance, 0, 0
       */
      average = Float32.floatToFloat32(0.0f)
      variance = Float32.floatToFloat32(0.0f)

      println(s"Enter $n samples")
      /* :start_inner_loop
       * CMPEQ $n, 0
       * JMP :end_inner_loop
       * READ $value
       * SUBF $tmp, $value, $oldAverage
       * ADDF $average, $average, $value
       * MULF $tmp, $tmp, $tmp
       * ADDF $variance, $variance, $tmp
       * JMP :start_inner_loop
       * :end_inner_loop
       */
      for (i <- 1 to n) {
        val value = Float32.floatToFloat32(Console.readFloat)
        val tmp = value - oldAverage

        average = average + value
        variance = variance + tmp * tmp
      }

      /* DIVF $average, $average, 4
       * DIVF $variance, $variance, 4
       * CALL :sqrt_func ; inline code of sqrt here
       */
      average = average / nf
      variance = variance / nf
      variance = variance.SQRT

      println(s"Average: ${Float32.float32ToFloat(average)}")
      println(s"Variance: ${Float32.float32ToFloat(variance)}")

      // Process and act

      // ADD $oldAverage, $average, 0
      oldAverage = average
    }
  }
}

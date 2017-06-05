import java.lang.Float

object Float32 {

  def floatToFloat32(x: Float): Float32 = {
    val bits: Int = Float.floatToRawIntBits(x)

    val sign:     Int = (bits & 0x80000000) >>> 31
    val exponent: Int = ((bits & 0x7f800000) >>> 23) - 127
    val mantissa: Int = (bits & 0x007fffff) >>> 0

    new Float32(sign, exponent, mantissa)
  }

  def float32ToFloat(f: Float32): Float = {
    val sign:     Int = f.sign     << 31
    val exponent: Int = (f.exponent + 127) << 23
    val mantissa: Int = f.mantissa <<  0

    val bits: Int = sign | exponent | mantissa

    Float.intBitsToFloat(bits)
  }

}

class Float32(var sign: Int, var exponent: Int, var mantissa: Int) {

  override def toString: String = {
    s"Sign: ${sign.toHexString}\tExponent: ${exponent.toHexString}\tMantissa: ${mantissa.toHexString}"
  }

  def toFloat(): Float = Float32.float32ToFloat(this)

  def *(that: Float32): Float32 = {
    /* AND $signA, $A, 0x80000000
     * AND $signB, $B, 0x80000000
     * XOR $sign, $signA, $signB
     */
    val sign     :Int = this.sign ^ that.sign
    /* ADD $exponent, $exponentA, $exponentB */
    var exponent :Int = this.exponent + that.exponent
    /* ADD $mantissa, $mantissaA, $mantissaB */
    var mantissa :Int = this.mantissa + that.mantissa

    /* ADD $blocker, 0x400000, 0x0 */
    var blocker :Int = 0x00400000

    /* ADD $i, 0x1, 0x0 */
    /* :start_of_loop
     * CMPGT $i, 23 ; if $i > 23
     * JMP :end_of_loop
     */
    for(i <- 1 to 23) {
      /* AND $tmp, $mantissaA, $blocker
       * CMPEQ $tmp, 0x0
       * JMP :after_inner_if
       * SHIFTR $tmp, $mantissaA, $i
       * ADD $mantissa, $mantissa, $tmp
       * :after_inner_if
       */
      if( (this.mantissa & blocker) != 0 ) {
        mantissa += (that.mantissa >>> i )
      }
      /* SHIFTR $blocker, $blocker, 1 */
      blocker = blocker >>> 1
    }
    /* JMP :start_of_loop
     * :end_of_loop
     */

    /* AND $tmp, $mantissa, 0x800000
     * CMPEQ $tmp, 0x0
     * JMP :after_if
     * ADD $exponent, $exponent, 1
     * AND $tmp, $mantissa, 0x7fffff
     * SHIFTR $mantissa, $mantissa, 1
     * :after_if
     */
    if( (mantissa & 0x00800000) != 0) {
      exponent += 1
      mantissa = (mantissa & 0x007fffff) / 2
    }

    new Float32(sign, exponent, mantissa)
  }

  def /(that: Float32): Float32 = {
    /* AND $signA, $A, 0x80000000
     * AND $signB, $B, 0x80000000
     * XOR $sign, $signA, $signB
     */
    val sign     :Int = this.sign ^ that.sign
    /* SUB $exponent, $exponentA, $exponentB */
    var exponent :Int = this.exponent - that.exponent
    /* ADD $mantissa, 0x0, 0x0 */
    var mantissa :Int = 0

    /* ADD $mantissaLeft, $mantissaA, 0x00800000
     * ADD $mantissaRight, $mantissaB, 0x00800000 */
    var mantissaLeft  :Int = this.mantissa + 0x00800000
    var mantissaRight :Int = that.mantissa + 0x00800000

    /* ADD $i, 0x1, 0x0 */
    /* :start_of_loop
     * CMPGT $i, 23 ; if $i > 23
     * JMP :end_of_loop
     */
    for(i <- 0 to 23) {
      /* CMPLT $mantissaLeft, $mantissaRight
       * JMP :after_inner_if
       * SUB $tmp, 23, $i
       * SHIFTL $tmp, 0x1, $tmp
       * ADD $mantissa, $mantissa, $tmp
       * SUB $mantissaLeft, $mantissaLeft, $mantissaRight
       * :after_inner_if
       */
      if( mantissaLeft >= mantissaRight ) {
        mantissa += (1 << (23 - i))
        mantissaLeft -= mantissaRight
      }
      /* SHIFTR $mantissaRight, $mantissaRight, 1 */
      mantissaRight = mantissaRight >>> 1
    }
    /* JMP :start_of_loop
     * :end_of_loop
     */

    /* AND $tmp, $mantissa, 0x800000
     * CMPNE $tmp, 0
     * JMP :after_if
     * SUB $exponent, $exponent, 1
     * SHIFTL $mantissa, $mantissa, 1
     * :after_if
     */
    if( (mantissa & 0x00800000) == 0) {
      exponent -= 1
      mantissa = mantissa << 1
    }

    new Float32(sign, exponent, mantissa & 0x007fffff)
  }

  def ADD(x: Int, y: Int): Int = x + y
  def SUB(x: Int, y: Int): (Int, Boolean, Boolean) = {
    val z = x - y
    val b1 = z < 0
    val b2 = z == 0

    (z, b1, b2)
  }

  def LT(x: Int, y: Int) = SUB(x, y)._2
  def EQ(x: Int, y: Int) = SUB(x, y)._3

  def SHIFTR(x: Int, p: Int): (Int, Int) = ((p << 31) | (x >>> 1), x & 0x1)
  def SHIFTL(x: Int, p: Int): (Int, Int) = ((x << 1) | p, (x & 0x80000000) >>> 31)

  // compare the absolutes values
  def ~<(that: Float32): Boolean = {
    /* CMPLT $exponantA, $exponantB
     * RET 1
     * CMPNE $exponantA, $exponantB
     * RET 0
     * CMPLT $mantissaA, $mantissaB
     * RET 1
     * RET 0
     */
    LT(this.exponent, that.exponent) ||
      (EQ(this.exponent, that.exponent) && LT(this.mantissa, that.mantissa))
  }

  def -(that: Float32): Float32 = {
    /* SUB $signB, $signB, 1 */
    that.sign = 1 - that.sign
    /* JMP :plus_func; tailrec call to `+` */
    this + that
  }

  // Normalize the numbers for the internal __add
  def +(that: Float32): Float32 = {
    var a: Float32 = this
    var b: Float32 = that

    // Reorder the numbers: bigger first
    /* Inline the call */
    if (this ~< that) {
      a = that
      b = this
    }

    /* AND $is_neg, $signA, $signB */
    val is_neg = (a.sign & b.sign) != 0;

    /* CMPEQ $is_neg, 0
     * JMP :after_if
     * ADD $signA, 0, 0
     * ADD $signB, 0, 0
     * :after_if
     */
    if (is_neg) {
      this.sign = 0
      that.sign = 0
    }

    /* ; inline code of `__add` */
    val c = a __add b

    /* CMPNE $is_neg, 0
     * ADD $sign, 1, 0 */
    if (is_neg) {
      c.sign = 1
    }

    /* RET */
    return c
  }

  // Suppose that both numbers are positive and that the second one is the smallest
  def __add(that: Float32): Float32 = {
    // Add MSB
    /* SHIFTL $tmp, 1, 23
     * OR $mantissaA, $mantissaA, $tmp
     * OR $mantissaB, $mantissaB, $tmp
     */
    this.mantissa = this.mantissa | (1 << 23)
    that.mantissa = that.mantissa | (1 << 23)

    /* CMPEQ $signA, 1
     * SUB $mantissaA, 0, $mantissaA
     */
    if (this.sign == 1)
      this.mantissa = -this.mantissa

    /* CMPEQ $sign1, 1
     * SUB $mantissa1, 0, $mantissa1
     */
    if (that.sign == 1)
      that.mantissa = -that.mantissa

    // Equalize the exponent
    while (!EQ(this.exponent, that.exponent)) {
      that.mantissa = SHIFTR(that.mantissa, 0)._1
      that.exponent = ADD(that.exponent, 1)
    }

    var sign = this.sign
    var exponent = this.exponent
    var mantissa = ADD(this.mantissa, that.mantissa)


    if (mantissa < 0)
      mantissa = -mantissa

    // Check overflow
    if ((mantissa & (1 << 24)) != 0) {
      mantissa = SHIFTR(mantissa, 0)._1
      exponent = ADD(exponent, 1)
    }

    // Find new MSB
    while ((mantissa & (1 << 23)) == 0 && mantissa != 0) {
      mantissa = SHIFTL(mantissa, 0)._1
      exponent = SUB(exponent, 1)._1
    }

    // Remove MSB
    mantissa = mantissa & 0x7fffff

    this.mantissa = this.mantissa & 0x7fffff
    that.mantissa = that.mantissa & 0x7fffff

    new Float32(sign, exponent, mantissa)
  }

  def SQRT(): Float32 = {
    var n: Int = 10
    var x: Float32 = this
    var thisf: Float = Float32.float32ToFloat(this)
    var xf: Float = thisf
    var two: Float32 = Float32.floatToFloat32(2.0f)

    // Newton's method
    while (n > 0) {
      x = (x + this / x) / two
      xf = (xf + thisf / xf) / 2.0f
      n -= 1;
    }

    x
  }

}

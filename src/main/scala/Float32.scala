import java.lang.Float

object Float32 {

  def floatToFloat32(x: Float): Float32 = {
    val bits: Int = Float.floatToRawIntBits(x)

    val sign:     Int = (bits & 0x80000000) >>> 31
    val exponent: Int = ((bits & 0x7f800000) >>> 23) - 127
    val mantissa: Int = (bits & 0x007fffff) >>>  0

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
    val sign     :Int = this.sign ^ that.sign
    var exponent :Int = this.exponent + that.exponent
    var mantissa :Int = this.mantissa + that.mantissa

    var blocker :Int = 0x00400000

    for(i <- 1 to 23) {
      if( (this.mantissa & blocker) != 0 ) {
        mantissa += (that.mantissa >>> i )
      }
      blocker = blocker >>> 1
    }

    if( (mantissa & 0x00800000) != 0) {
      exponent += 1
      mantissa = (mantissa & 0x007fffff) / 2
    }

    new Float32(sign, exponent, mantissa)
  }

  def /(that: Float32): Float32 = {
    val sign     :Int = this.sign ^ that.sign
    var exponent :Int = this.exponent - that.exponent
    var mantissa :Int = 0

    var mantissaLeft  :Int = this.mantissa + 0x00800000
    var mantissaRight :Int = that.mantissa + 0x00800000

    for(i <- 0 to 23) {
      if( mantissaLeft >= mantissaRight ) {
        mantissa += (1 << (23 - i))
        mantissaLeft -= mantissaRight
      }
      mantissaRight = mantissaRight >>> 1
    }

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

  def ~<(that: Float32): Boolean =
    LT(this.exponent, that.exponent) || LT(this.mantissa, that.mantissa)

  // Normalize the numbers for the internal __add
  def +(that: Float32): Float32 = {
    var a: Float32 = this
    var b: Float32 = that

    // Reorder the numbers: bigger first
    if (this ~< that) {
      a = that
      b = this
    }

    a __add b
  }

  // Suppose that both numbers are positive and that the second one is the smallest
  def __add(that: Float32): Float32 = {
    // Add MSB
    this.mantissa = this.mantissa | (1 << 23)
    that.mantissa = that.mantissa | (1 << 23)

    if (this.sign == 1)
      this.mantissa = -this.mantissa

    if (that.sign == 1)
      that.mantissa = -that.mantissa

    // Equalize the exponent
    while (!EQ(this.exponent, that.exponent)) {
      that.mantissa = SHIFTR(that.mantissa, 0)._1
      that.exponent = ADD(that.exponent, 1)
    }

    var sign = this.sign & that.sign
    var exponent = this.exponent
    var mantissa = ADD(this.mantissa, that.mantissa)

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
    mantissa = mantissa & 0x3fffff

    this.mantissa = this.mantissa & 0x3fffff
    that.mantissa = that.mantissa & 0x3fffff

    new Float32(sign, exponent, mantissa)
  }

}

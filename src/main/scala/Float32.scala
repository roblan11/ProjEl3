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

class Float32(val sign: Int, val exponent: Int, val mantissa: Int) {

  override def toString(): String = {
    s"Sign: ${sign.toHexString}\tExponent: ${exponent.toHexString}\tMantissa: ${mantissa.toHexString}"
  }

  def *(that: Float32): Float32 = {
    var mantissa  = this.mantissa + that.mantissa
    var blocker   = 0x00400000
    for(i <- 1 to 23) {
      if( (this.mantissa & blocker) != 0 ) {
        mantissa += (that.mantissa >>> i )
      }
      blocker = blocker >>> 1
    }
    val sign      = this.sign ^ that.sign
    var exponent  = this.exponent + that.exponent

    if( (mantissa & 0x00800000) != 0) {
      exponent += 1
      mantissa = (mantissa & 0x007fffff) / 2
    }

    new Float32(sign, exponent, mantissa)
  }

}

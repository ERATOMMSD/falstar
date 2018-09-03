package linear

case class Complex(real: Double, imag: Double) {
  def unary_- = {
    Complex(-real, -imag)
  }

  def +(that: Complex) = {
    Complex(this.real + that.real, this.imag + that.imag)
  }

  def -(that: Complex) = {
    Complex(this.real - that.real, this.imag - that.imag)
  }

  def +(that: Double) = {
    Complex(this.real + that, this.imag)
  }

  def -(that: Double) = {
    Complex(this.real - that, this.imag)
  }

  def *(that: Double) = {
    Complex(this.real * that, this.imag * that)
  }

  def /(that: Double) = {
    Complex(this.real / that, this.imag / that)
  }

  def *(that: Complex) = {
    val real = this.real * that.real - this.imag * that.imag
    val imag = this.real * that.imag + this.imag * that.real
    Complex(real, imag)
  }
}

object Compelx {
  val i = Complex(0, 1)
  val zero = Complex(0, 0)
  val one = Complex(1, 0)
}
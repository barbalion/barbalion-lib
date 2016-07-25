package com.barbalion.lib.math

import scala.language.implicitConversions
import scala.math._

case class Complex(re: Double, im: Double) extends Ordered[Complex] {
  private lazy val modulus = sqrt(pow(re, 2) + pow(im, 2))

  // Constructors
  def this(re: Double) = this(re, 0)

  // Unary operators
  def unary_+ = this

  def unary_- = new Complex(-re, -im)

  def unary_~ = new Complex(re, -im)

  // conjugate
  def unary_! = modulus

  // Comparison
  def compare(that: Complex) = !this compare !that

  // Arithmetic operations
  def +(c: Complex) = new Complex(re + c.re, im + c.im)

  def -(c: Complex) = this + -c

  def *(c: Complex) =
    new Complex(re * c.re - im * c.im, im * c.re + re * c.im)

  def /(c: Complex) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  // String representation
  override def toString =
    this match {
      case Complex.i => "i"
      case Complex(_re, 0) => _re.toString
      case Complex(0, _im) => _im.toString + "*i"
      case _ => asString
    }

  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

//noinspection NoReturnTypeForImplicitDef
object Complex {
  // Constants
  val i = new Complex(0, 1)

  // Factory methods
  def apply(re: Double) = new Complex(re)

  // Implicit conversions

  implicit def fromDouble(d: Double) = new Complex(d)

  implicit def fromFloat(f: Float) = new Complex(f)

  implicit def fromLong(l: Long) = new Complex(l)

  implicit def fromInt(i: Int) = new Complex(i)
}


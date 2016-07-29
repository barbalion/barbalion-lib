package com.barbalion.lib.math

import com.barbalion.lib.math.DoubleE.NaN

import scala.language.implicitConversions

/**
  * Error-based calculations
  * All operations calculates Error as a co-product
  * Each value also has
  */
case class DoubleE(
               // value
               value: Double,
               // squared error
               err2: Double
             ) extends Numeric[DoubleE] with Fractional[DoubleE] with Ordered[DoubleE] {
  // error
  lazy val err = Math.sqrt(err2)

  // squared value
  lazy val value2 = value * value

  // double-squared value
  lazy val value4 = value2 * value2

  // value with zero error
  def exact = newResultValue(value, 0)

  override def plus(x: DoubleE, y: DoubleE): DoubleE = newResultValueWithY(y)(
    x.value + y.value, x.err2 + y.err2)

  protected def newResultValueWithY(y: DoubleE)(value: Double, err2: Double) = withValueOf(y) {
    new DoubleE(value, err2)
  }

  private def withValueOf(value: DoubleE)(m: => DoubleE): DoubleE = value match {
    case NaN => NaN
    case _ => m
  }

  override def minus(x: DoubleE, y: DoubleE): DoubleE = newResultValueWithY(y)(
    x.value - y.value, x.err2 + y.err2)

  override def times(x: DoubleE, y: DoubleE): DoubleE = newResultValueWithY(y)(
    x.value * y.value, x.err2 * y.value2 + y.err2 * x.value2)

  override def fromInt(x: Int): DoubleE = newResultValue(x, 0)

  override def div(x: DoubleE, y: DoubleE): DoubleE = withValueOf(y) {
    if (y.value == 0) NaN else newResultValue(x.value / y.value, (x.err2 * y.value2 + y.err2 * x.value2) / y.value4)
  }

  def +(a: Int): DoubleE = this + a.toDouble

  def +(a: Double) = newResultValue(value + a, err2)

  protected def newResultValue(value: Double, err2: Double) = new DoubleE(value, err2)

  def -(a: Int): DoubleE = this - a.toDouble

  def -(a: Double) = newResultValue(value - a, err2)

  def *(a: Int): DoubleE = this * a.toDouble

  def *(a: Double) = newResultValue(value * a, err2 * (a * a))

  def /(a: Int): DoubleE = this / a.toDouble

  def /(a: Double) = if (a == 0) NaN else newResultValue(value / a, err2 / (a * a))

  def sqr = newResultValue(value2, 4 * err2 * value2)

  def sqrt = if (value < 0) NaN else newResultValue(Math.sqrt(value), if (value == 0) err2 / 4 else err2 / (4 * value))

  def exp = {
    val e: Double = math.exp(value)
    newResultValue(e, err2 * e * e)
  }

  // check if values match and ignore the errors
  def ==(a: DoubleE) = value == a.value

  def !=(a: DoubleE) = value != a.value

  def !==(a: DoubleE) = !(this === a)

  // check if both the values and the errors matches
  def ===(a: DoubleE) = value == a.value && err2 == a.err2

  override def toString: String = if (err == 0) value.toString else value.toString + "+-" + err.toString

  override def toInt(x: DoubleE): Int = value.toInt

  override def toLong(x: DoubleE): Long = value.toLong

  override def toFloat(x: DoubleE): Float = value.toFloat

  override def toDouble(x: DoubleE): Double = value.toDouble

  override def negate(x: DoubleE): DoubleE = newResultValue(-value, err2)

  override def equals(o: scala.Any): Boolean = o match {
    case a: DoubleE => value.equals(a.value) && err2.equals(a.err2)
    case _ => false
  }

  override def compare(x: DoubleE, y: DoubleE): Int = x.compare(y)

  override def compare(that: DoubleE): Int = value.compareTo(that.value)

  override def hashCode(): Int = value.hashCode() + err2.hashCode()

}

object DoubleE {
  def weightedMean(values: Seq[DoubleE]) = {
    values.filter(_.err2 == 0) match {
      case Nil => // weighted mean
        val x = values.map(x => x / x.err).sum(Zero)
        val y = values.map(1 / _.err).sum
        (x / y) + Err(mean(values).err2)
      case exactValues => // simple mean of value with zero error (ignore values with error)
        mean(exactValues)
    }
  }

  def mean(values: Seq[DoubleE]) = {
    def sqr(x: Double) = x * x
    val exactMean = values.map(_.value).sum / values.length
    val meanError2 = values.map(x => sqr(x.value - exactMean.value)).sum / values.length
    Err(meanError2) + exactMean
  }

  implicit def fromInt(a: Int): DoubleE = fromDouble(a)

  implicit def fromDouble(a: Double): DoubleE = a match {
    case 0.0 => Zero
    case 1.0 => One
    case 2.0 => Two
    case 3.0 => Three
    case 4.0 => Four
    case _ => new DoubleE(a, 0)
  }

  implicit def infixFractionalOps(x: DoubleE): DoubleE#FractionalOps = new x.FractionalOps(x)

  object Zero extends DoubleE(0, 0)

  object One extends DoubleE(1, 0)

  object Two extends DoubleE(2, 0)

  object Three extends DoubleE(3, 0)

  object Four extends DoubleE(4, 0)

  /* here comes NaN implementation */
  object NaN extends DoubleE(Double.NaN, Double.NaN) {

    override def toString: String = "NaN"

    override protected def newResultValue(value: Double, err2: Double): DoubleE = this

  }

  object Err {
    def apply(err2: Double) = new DoubleE(0, err2)
  }

}


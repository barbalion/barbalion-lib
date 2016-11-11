package com.barbalion.lib.math

import com.barbalion.lib.math.DoubleE.NaN

import scala.language.implicitConversions
import scala.util.Random

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
  /** error */
  lazy val err = Math.sqrt(err2)

  /** squared value */
  lazy val value2 = value * value

  /** double-squared value */
  lazy val value4 = value2 * value2

  /** value with zero error */
  def exact = newResultValue(value, value * value * DoubleE.DOUBLE_ERROR2)

  /** return the value with random error - normal (Gaussian) distribution */
  def normal = newResultValue(value + Random.nextGaussian * err, fixedErr2)

  override def plus(x: DoubleE, y: DoubleE): DoubleE = newResultValueWithY(y)(
    x.value + y.value, x.fixedErr2 + y.fixedErr2)

  protected def newResultValueWithY(y: DoubleE)(value: Double, err2: Double) = withValueOf(y) {
    newResultValue(value, err2)
  }

  private def withValueOf(value: DoubleE)(m: => DoubleE): DoubleE = value match {
    case NaN => NaN
    case _ => m
  }

  override def minus(x: DoubleE, y: DoubleE): DoubleE = newResultValueWithY(y)(
    x.value - y.value, x.fixedErr2 + y.fixedErr2)

  override def times(x: DoubleE, y: DoubleE): DoubleE = newResultValueWithY(y)(
    x.value * y.value, x.fixedErr2 * y.value2 + y.fixedErr2 * x.value2)

  override def fromInt(x: Int): DoubleE = newResultValue(x, x * x * DoubleE.DOUBLE_ERROR2)

  override def div(x: DoubleE, y: DoubleE): DoubleE = withValueOf(y) {
    newResultValue(x.value / y.value, (x.fixedErr2 * y.value2 + y.fixedErr2 * x.value2) / y.value4)
  }

  def +(a: Int): DoubleE = this + a.toDouble

  def +(a: Double) = newResultValue(value + a, fixedErr2)

  def isGoodNumber = !value.isInfinity && !value.isNaN

  protected lazy val fixedErr2 = {
    val minErr2 = value * value * DoubleE.DOUBLE_ERROR2
    if (err2.isNaN || err2 < minErr2) minErr2 else err2
  }

  protected def newResultValue(value: Double, err2: Double) = {
    new DoubleE(value, err2)
  }

  def -(a: Int): DoubleE = this - a.toDouble

  def -(a: Double) = newResultValue(value - a, fixedErr2)

  def *(a: Int): DoubleE = this * a.toDouble

  def *(a: Double) = newResultValue(value * a, fixedErr2 * (a * a))

  def /(a: Int): DoubleE = this / a.toDouble

  def /(a: Double) = if (a == 0) NaN else newResultValue(value / a, fixedErr2 / (a * a))

  def sqr = newResultValue(value2, 4 * fixedErr2 * value2)

  def sqrt = if (value < 0) NaN else newResultValue(Math.sqrt(value), if (value == 0) fixedErr2 / 4 else fixedErr2 / (4 * value))

  def exp = {
    val e: Double = math.exp(value)
    newResultValue(e, fixedErr2 * e * e)
  }

  def log = newResultValue(math.log(value), fixedErr2 / value / value)

  def sin = newResultValue(math.sin(value), fixedErr2 * { val v = math.cos(value); v * v })

  def cos = newResultValue(math.sin(value), fixedErr2 * { val v = math.sin(value); v * v })

  // check if values match and ignore the errors
  def ==(a: DoubleE) = value == a.value

  def !=(a: DoubleE) = value != a.value

  def !==(a: DoubleE) = !(this === a)

  // check if the values match within errors
  def ===(a: DoubleE) = (value - a.value) * (value - a.value) < fixedErr2 + a.fixedErr2

  override def toString: String = if (err == 0) value.toString else value.toString + "+-" + err.toString

  override def toInt(x: DoubleE): Int = value.toInt

  override def toLong(x: DoubleE): Long = value.toLong

  override def toFloat(x: DoubleE): Float = value.toFloat

  override def toDouble(x: DoubleE): Double = value.toDouble

  override def negate(x: DoubleE): DoubleE = newResultValue(-value, fixedErr2)

  override def equals(o: scala.Any): Boolean = o match {
    case a: DoubleE => value.equals(a.value) && fixedErr2.equals(a.fixedErr2)
    case _ => false
  }

  override def compare(x: DoubleE, y: DoubleE): Int = x.compare(y)

  override def compare(that: DoubleE): Int = value.compareTo(that.value)

  override def hashCode(): Int = value.hashCode() + err2.hashCode()

}

object DoubleE {
  def weightedMean(values: Traversable[DoubleE]): DoubleE = {
    values.filter(_.fixedErr2 == 0) match {
      case empty if empty.isEmpty => // weighted mean
        values.map(x => x / x.fixedErr2).sum(Zero) / values.map(1 / _.fixedErr2).sum
      case exactValues => // simple mean of value with zero error (ignore values with error)
        exactMean(exactValues)
    }
  }

  def exactMean(values: Traversable[DoubleE]): DoubleE = {
    def sqr(x: Double) = x * x

    val count = if (values.nonEmpty) values.size else 1
    val exactMean = (values map (_.value)).sum / count
    val meanError2 = values.map(x => sqr(x.value - exactMean)).sum / count
    DoubleE(exactMean, meanError2)
  }

  implicit def fromInt(a: Int): DoubleE = fromDouble(a)

  implicit def fromDouble2(v: (Double, Double)): DoubleE = DoubleE(v._1, v._2)

  implicit def fromDouble(a: Double): DoubleE = a match {
    case 0.0 => Zero
    case 1.0 => One
    case 2.0 => Two
    case 3.0 => Three
    case 4.0 => Four
    case _ => new DoubleE(a, a * a * DOUBLE_ERROR2)
  }

  val DOUBLE_ERROR2 = 1e-18 * 1e-18

  implicit def infixFractionalOps(x: DoubleE): DoubleE#FractionalOps = new x.FractionalOps(x)

  val Zero = DoubleE(0, DOUBLE_ERROR2)
  val One = DoubleE(1, DOUBLE_ERROR2)
  val Two = DoubleE(2, 4 * DOUBLE_ERROR2)
  val Three = DoubleE(3, 9 * DOUBLE_ERROR2)
  val Four = DoubleE(4, 16 * DOUBLE_ERROR2)

  /* here comes NaN implementation */
  object NaN extends DoubleE(Double.NaN, Double.NaN) {

    override def toString: String = "NaN"

    override protected def newResultValue(value: Double, err2: Double): DoubleE = this

  }

  object Err {
    def apply(err2: Double) = new DoubleE(0, err2)
  }

}


package com.barbalion.lib.math

import com.barbalion.lib.math.DoubleE.Zero

/**
  * Immutable vectors with Error
  */
abstract class VectorE[T <: VectorE[_]](val coordinates: List[DoubleE]) {
  // squared length
  lazy val r2: DoubleE = coordinates map {
    _.sqr
  } sum Zero

  // vector length
  lazy val r: DoubleE = r2.sqrt

  def +(a: VectorE[T]): VectorE[T] = combineVectors(a, Zero.plus)

  def -(a: VectorE[T]): VectorE[T] = combineVectors(a, Zero.minus)

  protected def combineVectors(a: VectorE[T], op: (DoubleE, DoubleE) => DoubleE): VectorE[T] = buildInstance(
    (coordinates lazyZip a.coordinates) map op
  )

  def buildInstance(coordinates: List[DoubleE]): VectorE[T]
}

case class Vector2E(x: DoubleE, y: DoubleE) extends VectorE(List(x, y)) {
  override def buildInstance(coordinates: List[DoubleE]): Vector2E = coordinates match {
    case x_ :: y_ :: Nil => Vector2E(x_, y_)
    case _ => throw new Exception("Bad vector.")
  }

  def +(a: Vector2E): Vector2E = combineVectors(a, Zero.plus).asInstanceOf[Vector2E]

  def -(a: Vector2E): Vector2E = combineVectors(a, Zero.minus).asInstanceOf[Vector2E]

  def /(div: DoubleE): Vector2E = Vector2E(x / div, y / div)

  def *(div: DoubleE): Vector2E = Vector2E(x * div, y * div)

  def ==(a: Vector2E): Boolean = x.value == a.x.value && y.value == a.y.value

  /**
    * returns the vector of the same direction and length = 1.0
    */
  lazy val normalize: Vector2E = /(r)

  def posE: (DoubleE, DoubleE) = (x, y)

  def pos: (Double, Double) = (x.value, y.value)

  def posFloat: (Float, Float) = (x.value.toFloat, y.value.toFloat)

  def reverse: Vector2E = Vector2E(-x, -y)

  def phi: Double = math.atan2(y.value, x.value)

  def isGoodVector: Boolean = x.isGoodNumber && y.isGoodNumber
}

object Vector2E {
  implicit def tuple2convE(v: (DoubleE, DoubleE)): Vector2E = Vector2E(v._1, v._2)

  implicit def tuple2conv(v: (Double, Double)): Vector2E = Vector2E(v._1, v._2)

  def weightedMean(vs: IterableOnce[Vector2E]): Option[Vector2E] = vs match {
    case empty if empty.iterator.isEmpty => None
    case _ => Some(Vector2E(DoubleE.weightedMean(vs.iterator.map(_.x)), DoubleE.weightedMean(vs.iterator.map(_.y))))
  }

  val Zero: Vector2E = Vector2E(DoubleE.Zero, DoubleE.Zero)
}

case class Vector3E(x: DoubleE, y: DoubleE, z: DoubleE) extends VectorE(List(x, y, z)) {
  override def buildInstance(coordinates: List[DoubleE]): Vector3E = coordinates match {
    case x_ :: y_ :: z_ :: Nil => Vector3E(x_, y_, z_)
    case _ => throw new Exception("Bad vector.")
  }

  def pos: (DoubleE, DoubleE, DoubleE) = (x, y, z)

  def posFloat: (Float, Float, Float) = (x.value.toFloat, y.value.toFloat, z.value.toFloat)
}


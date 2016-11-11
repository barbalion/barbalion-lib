package com.barbalion.lib.math

import com.barbalion.lib.math.DoubleE.Zero

/**
  * Immutable vectors with Error
  */
abstract class VectorE[T <: VectorE[_]](val coordinates: List[DoubleE]) {
  // squared length
  lazy val r2 = coordinates map {
    _.sqr
  } sum Zero

  // vector length
  lazy val r = r2.sqrt

  def +(a: VectorE[T]) = combineVectors(a, Zero.plus)

  def -(a: VectorE[T]) = combineVectors(a, Zero.minus)

  protected def combineVectors(a: VectorE[T], op: (DoubleE, DoubleE) => DoubleE) = buildInstance(
    (coordinates, a.coordinates).zipped map op
  )

  def buildInstance(coordinates: List[DoubleE]): VectorE[T]
}

case class Vector2E(x: DoubleE, y: DoubleE) extends VectorE(List(x, y)) {
  override def buildInstance(coordinates: List[DoubleE]) = coordinates match {
    case x_ :: y_ :: Nil => Vector2E(x_, y_)
    case _ => throw new Exception("Bad vector.")
  }

  def +(a: Vector2E) = combineVectors(a, Zero.plus).asInstanceOf[Vector2E]

  def -(a: Vector2E) = combineVectors(a, Zero.minus).asInstanceOf[Vector2E]

  def /(div: DoubleE) = Vector2E(x / div, y / div)

  def *(div: DoubleE) = Vector2E(x * div, y * div)

  def ==(a: Vector2E) = x.value == a.x.value && y.value == a.y.value

  /**
    * returns the vector of the same direction and length = 1.0
    */
  lazy val normalize = /(r)

  def posE = (x, y)

  def pos = (x.value, y.value)

  def posFloat = (x.value.toFloat, y.value.toFloat)

  def reverse = Vector2E(-x, -y)

  def phi = math.atan2(y.value, x.value)

  def isGoodVector = x.isGoodNumber && y.isGoodNumber
}

object Vector2E {
  implicit def tuple2convE(v: (DoubleE, DoubleE)): Vector2E = Vector2E(v._1, v._2)

  implicit def tuple2conv(v: (Double, Double)): Vector2E = Vector2E(v._1, v._2)

  def weightedMean(vs: Traversable[Vector2E]): Option[Vector2E] = vs match {
    case empty if empty.isEmpty => None
    case _ => Some(Vector2E(DoubleE.weightedMean(vs map (_.x)), DoubleE.weightedMean(vs map (_.y))))
  }

  val Zero = Vector2E(DoubleE.Zero, DoubleE.Zero)
}

case class Vector3E(x: DoubleE, y: DoubleE, z: DoubleE) extends VectorE(List(x, y, z)) {
  override def buildInstance(coordinates: List[DoubleE]) = coordinates match {
    case x_ :: y_ :: z_ :: Nil => Vector3E(x_, y_, z_)
    case _ => throw new Exception("Bad vector.")
  }

  def pos = (x, y, z)

  def posFloat = (x.value.toFloat, y.value.toFloat, z.value.toFloat)
}


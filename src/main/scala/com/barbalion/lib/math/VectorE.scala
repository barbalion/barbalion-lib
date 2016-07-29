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

  private def combineVectors(a: VectorE[T], op: (DoubleE, DoubleE) => DoubleE) = buildInstance(
    (coordinates, a.coordinates).zipped map op
  )

  def buildInstance(coordinates: List[DoubleE]): VectorE[T]
}

case class Vector2E(x: DoubleE, y: DoubleE) extends VectorE(List(x, y)) {
  override def buildInstance(coordinates: List[DoubleE]) = coordinates match {
    case x_ :: y_ :: Nil => Vector2E(x_, y_)
    case _ => throw new Exception("Bad vector.")
  }

  def pos = (x.value, y.value)
  def posFloat = (x.value.toFloat, y.value.toFloat)
}

case class Vector3E(x: DoubleE, y: DoubleE, z: DoubleE) extends VectorE(List(x, y, z)) {
  override def buildInstance(coordinates: List[DoubleE]) = coordinates match {
    case x_ :: y_ :: z_ :: Nil => Vector3E(x_, y_, z_)
    case _ => throw new Exception("Bad vector.")
  }

  def pos = (x, y, z)
  def posFloat = (x.value.toFloat, y.value.toFloat, z.value.toFloat)
}


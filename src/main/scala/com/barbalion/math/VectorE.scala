package com.barbalion.math

import com.barbalion.math.DoubleE.Zero

/**
  * Immutable vectors with Error
  */
abstract class VectorE[T <: VectorE[_]](val coordinates: List[DoubleE]) extends Aged {
  // squared length
  lazy val r2 = coordinates map {
    _.sqr
  } sum Zero

  // vector length
  lazy val r = r2.sqrt

  def generation: Long = coordinates.map {
    _.generation
  }.max

  def +(a: VectorE[T]) = combineVectors(a, Zero.plus)

  def -(a: VectorE[T]) = combineVectors(a, Zero.minus)

  private def combineVectors(a: VectorE[T], op: (DoubleE, DoubleE) => DoubleE) = buildInstance(
    (coordinates, a.coordinates).zipped map op
  )

  def buildInstance(coordinates: List[DoubleE]): VectorE[T]
}

class Vector2E(val x: DoubleE, val y: DoubleE) extends VectorE(List(x, y)) {
  override def buildInstance(coordinates: List[DoubleE]) = coordinates match {
    case x_ :: y_ :: Nil => new Vector2E(x_, y_)
    case _ => ???
  }
}

class Vector3E(val x: DoubleE, val y: DoubleE, val z: DoubleE) extends VectorE(List(x, y, z)) {
  override def buildInstance(coordinates: List[DoubleE]) = coordinates match {
    case x_ :: y_ :: z_ :: Nil => new Vector3E(x_, y_, z_)
    case _ => ???
  }
}


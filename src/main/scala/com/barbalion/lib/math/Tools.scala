package com.barbalion.lib.math

object Tools {
  def length(x: Float, y: Float) = math.sqrt(x * x + y * y).toFloat

  def length(v: (Float, Float)) = length(v._1, v._2)

  def length(x: Double, y: Double) = math.sqrt(x * x + y * y)

  def length(v: (Double, Double)) = length(v._1, v._2)

  def distance(p1: (Float, Float), p2: (Float, Float)) = length(p1._1 - p2._1, p1._2 - p2._2)

  def distance(p1: (Double, Double), p2: (Double, Double)) = length(p1._1 - p2._1, p1._2 - p2._2)
}

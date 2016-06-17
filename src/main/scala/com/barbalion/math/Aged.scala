package com.barbalion.math

/**
  * Created by barbalion on 15.02.2016.
  */
trait Aged {
  def generation: Long

  def combineGen(a: Aged) = generation.max(a.generation)
}

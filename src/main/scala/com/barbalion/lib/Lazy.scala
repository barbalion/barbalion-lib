package com.barbalion.lib

/**
  * Lazy calculated value.
  * Can recalculate if was invalidated by <code>invalidate()</code>.
  * Not thread safe.
  */
class Lazy[T](calc: () => T, onInvalidate: Option[() => Unit]) {
  def apply(): T = value.v

  def invalidate(): Unit = {
    value = new V
    onInvalidate.foreach(m => m())
  }

  private class V {
    // rely on Scala's built-in laziness
    lazy val v: T = calc()
  }

  @volatile
  private var value = new V

}

object Lazy {
  def apply[T](calc: () => T) = new Lazy(calc, None)

  def apply[T](calc: () => T, onCalc: () => Unit) = new Lazy(calc, Some(onCalc))
}
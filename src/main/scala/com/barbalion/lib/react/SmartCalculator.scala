package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Smart calculator to process circular dependencies (with some overhead).
  */
class SmartCalculator extends Calculator {
  private val calcStack = mutable.Set[Reactive[_]]()
  private var queue = mutable.Set[Reactive[_]]()

  override def calc(r: Reactive[_]): Unit = {
    calcStack.clear()
    reCalc(r)
  }

  override def reCalc(r: Reactive[_]): Unit = {
    if (calcStack.contains(r)) {
      queue += r
    } else {
      calcStack += r
      r.doCalc()
      calcStack -= r
    }
  }

  /**
    * Check if the calculation was completed (no circular dependencies found).
    *
    * @return
    */
  def done: Boolean = queue.isEmpty

  /**
    * Continues calculation if it wasn't [[done]].
    *
    * @return true if some calculation was performed
    */
  def continue(): Boolean = {
    if (queue.nonEmpty) {
      calcStack.clear()
      val oldQueue = queue
      queue = mutable.Set[Reactive[_]]()
      oldQueue foreach reCalc
      true
    } else
      false
  }
}

object SmartCalculator extends SmartCalculator {
  implicit val calculator = this
}
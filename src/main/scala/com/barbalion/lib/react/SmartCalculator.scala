package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Smart calculator to process circular dependencies (with some overhead and synchronization).
  */
class SmartCalculator extends Calculator {
  protected val calcStack = mutable.Set[Reactive[_]]()
  protected val queue = mutable.Set[Reactive[_]]()

  override def calc(r: Reactive[_]): Unit = synchronized {
    reCalc(r)
  }

  override def reCalc(r: Reactive[_]): Unit = synchronized {
    if (calcStack.contains(r)) {
      queue += r
    } else {
      calcStack += r
      try {
        r.doCalc()
      } finally {
        calcStack -= r
      }
    }
  }

  override def reCalc(rs: TraversableOnce[Reactive[_]]): Unit = rs foreach reCalc

  /**
    * Check if the calculation was completed (no circular dependencies found).
    *
    * @return
    */
  def done: Boolean = queue.isEmpty

  /**
    * Continues calculation if it wasn't [[done]].
    *
    * @return number of value recalculated
    */
  def continue(): Int = synchronized {
    if (queue.nonEmpty) {
      queue.clone() foreach (r => {
        queue -= r
        reCalc(r)
      })
      queue.size
    } else
      0
  }

  def clearQueue() = synchronized { queue.clear() }

}

object SmartCalculator extends SmartCalculator
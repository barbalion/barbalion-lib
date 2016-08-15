package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Smart calculator to process circular dependencies (with some overhead and synchronization).
  */
class SmartCalculator extends Calculator with QueuedCalculator {
  protected val calcStack = mutable.Set[Reactive[_]]()

  override def valueSet(r: Reactive[_]): Unit = r.invalidate()

  override def firstCalc(r: Reactive[_]): Unit = {
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

  override def reCalc(r: Reactive[_]): Unit = r.invalidate()

  override protected def continueQueue(queue: TraversableOnce[Reactive[_]]): Unit = queue foreach reCalc
}

object SmartCalculator extends SmartCalculator
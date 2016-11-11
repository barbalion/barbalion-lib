package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Smart calculator to process circular dependencies (with some overhead and synchronization).
  */
class SmartCalculator extends Calculator with QueuedCalculator {
  protected val calcStack = mutable.Set[Reactive[_]]()

  override def valueSet(r: Reactive[_]): Unit = r.invalidate()

  override def valueFirstRead(r: Reactive[_]): Unit = {
    if (calcStack.contains(r)) {
      enqueue(r)
    } else {
      calcStack += r
      try {
        r.doCalc()
      } finally {
        calcStack -= r
      }
    }
  }

  override def needReCalc(r: Reactive[_]): Unit = r.invalidate()

  override protected def continueQueue(queue: TraversableOnce[Reactive[_]]): Unit = queue foreach needReCalc
}
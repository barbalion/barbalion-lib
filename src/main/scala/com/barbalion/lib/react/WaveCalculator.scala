package com.barbalion.lib.react

/**
  * Calculates only one level of dependencies at once. Call continue() to calc the rest. Works fine with circular dependencies.
  */
class WaveCalculator extends Calculator with QueuedCalculator {

  override def valueSet(r: Reactive[_]): Unit = r.doCalc()

  override def valueFirstRead(r: Reactive[_]): Unit = r.doCalc()

  override def needReCalc(r: Reactive[_]): Unit = enqueue(r)

  override def continueQueue(queue: IterableOnce[Reactive[_]]): Unit = queue.iterator foreach (_.doCalc())

}


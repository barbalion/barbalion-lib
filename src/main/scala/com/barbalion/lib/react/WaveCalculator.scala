package com.barbalion.lib.react

/**
  * Calculates only one level of dependencies at once. Call continue() to calc the rest. Works fine with circular dependencies.
  */
class WaveCalculator extends Calculator with QueuedCalculator {

  override def valueSet(r: Reactive[_]): Unit = r.doCalc()

  override def firstUse(r: Reactive[_]): Unit = r.doCalc()

  override def reCalc(r: Reactive[_]): Unit = queue += r

  override def continueQueue(queue: TraversableOnce[Reactive[_]]): Unit = queue foreach (_.doCalc())

}


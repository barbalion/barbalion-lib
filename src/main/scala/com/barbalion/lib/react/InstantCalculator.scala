package com.barbalion.lib.react

/**
  * Simple calculator, calculates the reactive value instantly.
  *
  * @see Consider using [[com.barbalion.lib.react.SmartCalculator]] if you have circular dependencies.
  */
object InstantCalculator extends Calculator {
  override def valueSet(r: Reactive[_]): Unit = r.doCalc()

  override def firstCalc(r: Reactive[_]): Unit = r.doCalc()

  override def reCalc(r: Reactive[_]): Unit = r.doCalc()
}

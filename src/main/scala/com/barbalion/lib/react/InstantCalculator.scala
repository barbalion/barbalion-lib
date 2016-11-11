package com.barbalion.lib.react

/**
  * Simple calculator, calculates the reactive value instantly.
  *
  * @see Consider using [[com.barbalion.lib.react.SmartCalculator]] if you have circular dependencies.
  */
object InstantCalculator extends Calculator {
  override def valueSet(r: Reactive[_]): Unit = r.doCalc()

  override def valueFirstRead(r: Reactive[_]): Unit = r.doCalc()

  override def needReCalc(r: Reactive[_]): Unit = r.doCalc()
}

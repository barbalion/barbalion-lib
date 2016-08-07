package com.barbalion.lib.react

/**
  * Simple calculator, calculates the reactive value instantly.
  *
  * @see Consider using [[com.barbalion.lib.react.SmartCalculator]] if you have circular dependencies.
  */
object InstantCalculator extends Calculator {
  override def calc(r: Reactive[_]): Unit = r.doCalc()

  override def reCalc(r: Reactive[_]): Unit = r.doCalc()

  override def reCalc(rs: TraversableOnce[Reactive[_]]): Unit = rs foreach (_.doCalc())
}

package com.barbalion.lib.react

/**
  * The 99%-cases calculator. Cann't process circular dependencies but defers the real calculation before first value use.
  */
class LazyCalculator extends Calculator {
  override def valueSet(r: Reactive[_]): Unit = r.invalidate()

  override def valueFirstRead(r: Reactive[_]): Unit = r.doCalc()

  override def needReCalc(r: Reactive[_]): Unit = r.invalidate()
}
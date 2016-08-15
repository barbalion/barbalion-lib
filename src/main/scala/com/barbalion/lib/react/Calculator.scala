package com.barbalion.lib.react

/** Base for reactive Calculators
  *
  * @see [[com.barbalion.lib.react.InstantCalculator]]
  * @see [[com.barbalion.lib.react.SmartCalculator]]
  */
abstract class Calculator {
  def valueSet(r: Reactive[_]): Unit

  def firstCalc(r: Reactive[_]): Unit

  def reCalc(r: Reactive[_]): Unit
}










package com.barbalion.lib.react

/** Base for reactive Calculators
  * @see [[com.barbalion.lib.react.InstantCalculator]]
  * @see [[com.barbalion.lib.react.SmartCalculator]]
  */
abstract class Calculator {
  def calc(r: Reactive[_])
  def reCalc(r: Reactive[_])
}










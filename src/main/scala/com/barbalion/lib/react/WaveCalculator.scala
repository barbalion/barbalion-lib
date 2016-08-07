package com.barbalion.lib.react

import scala.collection.mutable

class WaveCalculator extends Calculator {
  protected val set = mutable.Set[Reactive[_]]()
  protected val queue = mutable.Queue[Reactive[_]]()

  override def calc(r: Reactive[_]): Unit = {
    if (set.add(r))
      queue += r
  }

  override def reCalc(r: Reactive[_]): Unit = calc(r)

  override def reCalc(rs: TraversableOnce[Reactive[_]]): Unit = rs foreach calc

  def continue() = {
    val clone = queue.clone
    queue.clear()
    set.clear()
    clone foreach (_.doCalc())
  }

  def done = queue.isEmpty
}

object WaveCalculator extends WaveCalculator

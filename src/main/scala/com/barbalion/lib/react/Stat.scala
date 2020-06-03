package com.barbalion.lib.react

trait Stat extends LazyCalculator {

  object stat {
    var totalSet = 0
    var totalFirstRead = 0
    var totalReCalc = 0
  }

  override def valueSet(r: Reactive[_]): Unit = {
    stat.totalSet += 1
    super.valueSet(r)
  }

  override def valueFirstRead(r: Reactive[_]): Unit = {
    stat.totalFirstRead += 1
    super.valueFirstRead(r)
  }

  override def needReCalc(r: Reactive[_]): Unit = {
    stat.totalReCalc += 1
    super.needReCalc(r)
  }
}

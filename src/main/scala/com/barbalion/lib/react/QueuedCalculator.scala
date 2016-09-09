package com.barbalion.lib.react
import scala.collection.mutable

/**
  * Trait for calculators with unordered queue
  */
trait QueuedCalculator extends Calculator {

  protected val queue = mutable.Set[Reactive[_]]()

  /** Continues calculation if it wasn't [[done]].
    * @return number of cells recalculated
    */
  def continue(): Int = {
    val clone = queue.clone
    queue.clear()
    continueQueue(clone)
    clone.size
  }

  protected def continueQueue(queue: TraversableOnce[Reactive[_]]): Unit

  /** Check if the calculation was completed (no circular dependencies found).
    * @return true if calculation queue is empty
    */
  def done = queue.isEmpty

  def clearQueue() = synchronized { queue.clear() }
}

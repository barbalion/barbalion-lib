package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Trait for calculators with unordered queue
  */
trait QueuedCalculator extends Calculator {

  protected val queue = new mutable.WeakHashMap[Reactive[_], Any]()

  /**
    * Continues calculation if it wasn't [[done]].
    * @return number of cells recalculated
    */
  def continue(): Int = {
    val clone = queue.clone.keys
    queue.clear()
    continueQueue(clone)
    clone.size
  }

  /**
    * Add item to the queue for future calculation
    * @param r the item to enqueue
    * @return <code>true</code> if the element wasn't in the queue yet
    */
  def enqueue(r: Reactive[_]) = queue.put(r, true).isEmpty

  protected def continueQueue(queue: TraversableOnce[Reactive[_]]): Unit

  /**
    * Check if the calculation was completed (no circular dependencies found).
    * @return true if calculation queue is empty
    */
  def done = queue.isEmpty

  /**
    * The queue length of the calculator
    * @return
    */
  def pending = queue.size

  def clearQueue() = synchronized { queue.clear() }
}

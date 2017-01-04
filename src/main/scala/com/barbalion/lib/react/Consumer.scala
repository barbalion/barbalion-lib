package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Trait that can consume changes (subscribe) for reactive changes of [[com.barbalion.lib.react.Producer Producer]]s.
  * Typically a cell is a consumer of other cells.
  *
  * Producers are weakly referenced here.
  */
trait Consumer {
  /** List of Producers */
  protected val producers = new mutable.WeakHashMap[Producer[_], Any]

  /** Consume (subscribe) the values of the [[com.barbalion.lib.react.Producer Producer]]s
    *
    * @param ps producers to subscribe
    */
  protected[react] def consume(ps: TraversableOnce[Producer[_]]): Unit = producers.synchronized({
    ps.foreach((p) => {
      p.subscribe(this)
      producers(p) = true
    })
  })

  /** Consume (subscribe) the values of the [[Producer Producer]]
    *
    * @param p producer to subscribe
    */
  protected[react] def consume(p: Producer[_]): Unit = consume(p :: Nil)

  /** Unsubscribe from all Producers. So no other's changes will affect this Consumer any more.
    */
  protected def unsubscribeFromAll(): Unit = {
    producers.foreach(_._1.unsubscribe(this))
    producers.clear()
  }

  /** Un-[[consume]] the producer. Unbind it from the producer so it changes will never affect this Consumer any more.
    *
    * @param p producer unbind
    * @return true if producer was consumed
    */
  protected def unsubscribeFrom(p: Producer[_]): Boolean = {
    p.unsubscribe(this)
    producers.remove(p).isDefined
  }

  /** Notification implementation. Will be invoked by producers on changes.
    *
    * @param p the producer which value changed
    */
  protected[react] def onProducerChange(p: Producer[_])

}

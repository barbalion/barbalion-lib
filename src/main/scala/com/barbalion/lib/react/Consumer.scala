package com.barbalion.lib.react

import scala.collection.mutable

/** Trait that can consume changes (subscribe) for reactive changes of [[com.barbalion.lib.react.Producer Producer]]s.
  * Typically a cell is a consumer of other cells.
  */
trait Consumer {
  /** List of Producers */
  private val producers = new mutable.HashSet[Producer[_]]

  /** Consume (subscribe) the values of the [[com.barbalion.lib.react.Producer Producer]]s
    *
    * @param ps producers to subscribe
    */
  protected[react] def consume(ps: Seq[Producer[_]]): Unit = producers.synchronized({
    ps.foreach((p) => {
      p.subscribe(this)
      producers.add(p)
    })
  })

  /** Consume (subscribe) the values of the [[Producer Producer]]
    *
    * @param p producer to subscribe
    */
  protected[react] def consume(p: Producer[_]): Unit = consume(p :: Nil)

  /** Unsubscribe from all Producers
    */
  protected def unConsumeAll(): Unit = {
    producers.foreach(_.unsubscribe(this))
    producers.clear()
  }
  /** Notification implementation. Will be invoked by producers on changes.
    * @param p the producer which value changed
    */
  protected[react] def producerChanged(p: Producer[_])
}

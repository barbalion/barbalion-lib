package com.barbalion.lib.react

import scala.collection.mutable

/**
  * Trait that can produce changes and notify [[com.barbalion.lib.react.Consumer Consumer]]s.
  * Typically a cell is a producer for other cells.
  */
trait Producer[T] {

  /**
    * Set of consumers
    */
  protected val subscribers = new mutable.HashSet[Consumer]

  /**
    * The produced value
    *
    * @return the value
    */
  def value: T

  /**
    * Subscribes new consumer for the notifications
    *
    * @param c Consumer to notify
    * @return true if consumer wasn't subscribed yet
    */
  protected[react] def subscribe(c: Consumer): Unit = subscribers.synchronized(subscribers.add(c))

  /**
    * Subscribes a simple one-time call-back
    *
    * @param c       Consumer to notify
    * @param oneTime if true then the callback will be called only once
    * @return true if consumer wasn't subscribed yet
    */
  //noinspection ConvertExpressionToSAM
  def subscribe(c: => Unit, oneTime: Boolean = false): Unit = subscribe(new Consumer {
    override protected[react] def onProducerChange(p: Producer[_]): Unit = {
      if (oneTime) unsubscribe(this)
      c
    }
  })

  /**
    * Unsubscribes the [[com.barbalion.lib.react.Consumer Consumer]] from the notifications
    *
    * @param c Consumer to unsubscribe
    * @return true if consumer was subscribed
    */
  protected[react] def unsubscribe(c: Consumer): Boolean = subscribers.synchronized(subscribers.remove(c))

  /**
    * Notify previously subscribed consumers
    */
  protected[react] final def notifyConsumers(): Unit = doNotifyConsumers(subscribers)

  protected[react] def doNotifyConsumers(consumers: mutable.HashSet[Consumer]): Unit = consumers.foreach(_.onProducerChange(this))

}

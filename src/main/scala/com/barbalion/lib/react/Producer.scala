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
  private val consumers = new mutable.HashSet[Consumer]

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
  protected[react] def subscribe(c: Consumer) = consumers.synchronized(consumers.add(c))

  /**
    * Unsubscribes the consumer from the notifications
    *
    * @param c Consumer to unsubscribe
    * @return true if consumer was subscribed
    */
  protected[react] def unsubscribe(c: Consumer) = consumers.synchronized(consumers.remove(c))

  /**
    * Notify previously subscribed consumers
    */
  protected[react] final def notifyConsumers() = doNotifyConsumers(consumers)

  protected[react] def doNotifyConsumers(consumers: mutable.HashSet[Consumer]) = consumers.foreach(_.producerChanged(this))

}

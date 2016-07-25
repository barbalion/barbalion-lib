package com.barbalion.lib.math

import scala.collection.mutable

/**
  * Trait that can produce changes and notify [[com.barbalion.lib.math.Consumer Consumer]]s.
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
  protected[math] def subscribe(c: Consumer) = consumers.synchronized(consumers.add(c))

  /**
    * Unsubscribes the consumer from the notifications
    *
    * @param c Consumer to unsubscribe
    * @return true if consumer was subscribed
    */
  protected[math] def unsubscribe(c: Consumer) = consumers.synchronized(consumers.remove(c))

  /**
    * Notify previously subscribed consumers
    */
  protected[math] def notifyConsumers() = consumers.foreach(_.notify(this))

}

/** Trait that can consume changes (subscribe) for reactive changes of [[com.barbalion.lib.math.Producer Producer]]s.
  * Typically a cell is a consumer of other cells.
  */
trait Consumer {
  /** List of Producers */
  private val producers = new mutable.HashSet[Producer[_]]

  /** Consume (subscribe) the values of the [[com.barbalion.lib.math.Producer Producer]]s
    *
    * @param ps producers to subscribe
    */
  protected[math] def consume(ps: Seq[Producer[_]]) = ps.synchronized({
    producers.foreach(_.unsubscribe(this))
    producers.clear()
    ps.foreach((p) => {
      p.subscribe(this)
      producers.add(p)
    })
  })

  /** Notification implementation. Will be invoked by producers on changes.
    *
    * @param p the producer which value changed
    */
  protected[math] def notify(p: Producer[_])
}

/** Reactive Cell implementation. It can store one [[com.barbalion.lib.math.Reactive#value() value]] of type <code>T</code>.
  * The value can be dependant of other cells.
  * Use [[com.barbalion.lib.math.Reactive$ Reactive]] object to create new instances of the cells.
  * Assign new values (constant or reactive) to <code>value</code> property.
  *
  * @tparam T the type of cell's value
  * @see [[com.barbalion.lib.math.Reactive#value value]] property
  */
abstract class Reactive[T] extends Producer[T] with Consumer {
  /** Cached last known value */
  protected var lastValue: T = initValue
  /** The variable function that will calculate the result for us */
  protected var calc: () => T = () => initValue

  /** Set constant value to the cell
    * No any trigger will affect this constant value.
    *
    * @param v the value
    */
  def value_=(v: T) = {
    calc = () => v
    consume(Nil)
    reCalc()
  }

  /** Syntax sugar to spawn new dependent reactive cell
    *
    * @param f function to calculate result
    * @tparam V type of the result
    * @return new <code>Reactive[V]</code> object
    */
  @inline def >>[V](f: (T) => V) = Reactive(this :: Nil, (l: Seq[T]) => f(value))

  /** The produced value
    *
    * @return current value of the cell
    */
  override def value: T = lastValue

  /** Set reactive value to the cell. It will automatically re-calculate if producer (i.e. other cells) changes.
    *
    * @param v a [[scala.Tuple2 Tuple2]] object with list of [[com.barbalion.lib.math.Producer producers]] and the calculation function.
    *          Use syntax sugar to create to Tuple2.
    * @tparam V the type of producers
    */
  //
  def value_=[V](v: (Seq[Producer[V]], Seq[V] => T)): Unit = {
    val (producers, fun) = v
    calc = () => fun(producers.map(_.value))
    consume(producers)
    reCalc()
  }

  /** Recalculates and assign new value */
  protected def reCalc() = {
    changeValue(calc())
  }

  /** Assigned new value and trigger notification if it was changed
    *
    * @param v new value
    */
  private def changeValue(v: T) = {
    if (isDifferent(v, lastValue)) {
      lastValue = v
      notifyConsumers()
    }
  }

  /** Compares two value to trigger notification if the value changed. To be overriden in descendants.
    *
    * @param newValue  new calculated value
    * @param lastValue previous know value
    * @return <code>true</code> if the values differ
    */
  protected def isDifferent(newValue: T, lastValue: T) = newValue != lastValue

  /** Syntax sugar to get dependent reactive value
    *
    * @param f function to calculate result
    * @tparam V type of the result
    * @return returns Tuple-object to be assigned to <code>value</code> property of <code>Reactive[V]</code>
    */
  @inline def apply[V](f: (T) => V) = (Seq(this), (l: Seq[T]) => f(value))

  /** Unsubscribe from all producers and keep last value, so no new recalculation of the value will occur.
    */
  @inline def unbind() = value = lastValue

  override protected[math] def notify(p: Producer[_]): Unit = reCalc()

  /** Initial value of the <code>Reactive</code> cell */
  protected def initValue: T

}

/** Implicit conversions and syntax sugar object for [[com.barbalion.lib.math.Reactive Reactive]] class.
  * Use [[com.barbalion.lib.math.Reactive#apply(scala.Function1) apply]] to spawn new Reactive[T] objects. */
//noinspection LanguageFeature
object Reactive {
  /**
    * Creates [[com.barbalion.lib.math.Reactive Reactive]] cell object with specified initial value.
    * @param v init value of the cell
    * @tparam T type of the value
    * @return new instance of Reactive object
    */
  def apply[T](v: T) = new Reactive[T] {
    override protected def initValue: T = v
  }

  /**
    * Creates [[com.barbalion.lib.math.Reactive Reactive]] cell object with specified initial dependency.
    * @param producers list of source value [[com.barbalion.lib.math.Producer Producer]]s
    * @param fun value calculation function
    * @tparam T type of the result value
    * @tparam V type of source values
    * @return new instance of Reactive object calculated from sources.
    */
  def apply[T, V](producers: Seq[Reactive[V]], fun: Seq[V] => T) = new Reactive[T] {
    override protected def initValue: T = fun(producers map (_.value))

    value = (producers, fun)
  }

  implicit def reactTupleConv[T1, T2](t: (Reactive[T1], Reactive[T2])): ReactiveTuple2[T1, T2] = new ReactiveTuple2(t)

  implicit def reactTupleConv[T1, T2, T3](t: (Reactive[T1], Reactive[T2], Reactive[T3])): ReactiveTuple3[T1, T2, T3] = new ReactiveTuple3(t)

  implicit def reactTupleConv[T1, T2, T3, T4](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4])): ReactiveTuple4[T1, T2, T3, T4] = new ReactiveTuple4(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5])): ReactiveTuple5[T1, T2, T3, T4, T5] = new ReactiveTuple5(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6])): ReactiveTuple6[T1, T2, T3, T4, T5, T6] = new ReactiveTuple6(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7])): ReactiveTuple7[T1, T2, T3, T4, T5, T6, T7] = new ReactiveTuple7(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8])): ReactiveTuple8[T1, T2, T3, T4, T5, T6, T7, T8] = new ReactiveTuple8(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9])): ReactiveTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new ReactiveTuple9(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10])): ReactiveTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new ReactiveTuple10(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11])): ReactiveTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new ReactiveTuple11(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12])): ReactiveTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new ReactiveTuple12(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13])): ReactiveTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new ReactiveTuple13(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14])): ReactiveTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new ReactiveTuple14(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15])): ReactiveTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new ReactiveTuple15(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16])): ReactiveTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new ReactiveTuple16(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17])): ReactiveTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new ReactiveTuple17(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18])): ReactiveTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new ReactiveTuple18(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19])): ReactiveTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new ReactiveTuple19(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20])): ReactiveTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new ReactiveTuple20(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21])): ReactiveTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new ReactiveTuple21(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21], Reactive[T22])): ReactiveTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new ReactiveTuple22(t)

  class ReactiveList[T](l: Seq[Reactive[T]]) {
    @inline def >>[V](f: Seq[T] => V) = Reactive(l, (a: Seq[T]) => f(a))

    @inline def apply[V](f: List[T] => V) = (l, f)
  }

  class ReactiveTuple2[T1, T2](t: (Reactive[T1], Reactive[T2])) {
    @inline def >>[V](f: (T1, T2) => V) = Reactive(Seq(t._1, t._2).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value))

    @inline def apply[V](f: (T1, T2) => V) = (Seq(t._1, t._2).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value))
  }

  class ReactiveTuple3[T1, T2, T3](t: (Reactive[T1], Reactive[T2], Reactive[T3])) {
    @inline def >>[V](f: (T1, T2, T3) => V) = Reactive(Seq(t._1, t._2, t._3).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value))

    @inline def apply[V](f: (T1, T2, T3) => V) = (Seq(t._1, t._2, t._3).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value))
  }

  class ReactiveTuple4[T1, T2, T3, T4](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4])) {
    @inline def >>[V](f: (T1, T2, T3, T4) => V) = Reactive(Seq(t._1, t._2, t._3, t._4).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value))

    @inline def apply[V](f: (T1, T2, T3, T4) => V) = (Seq(t._1, t._2, t._3, t._4).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value))
  }

  class ReactiveTuple5[T1, T2, T3, T4, T5](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5) => V) = (Seq(t._1, t._2, t._3, t._4, t._5).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value))
  }

  class ReactiveTuple6[T1, T2, T3, T4, T5, T6](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value))
  }

  class ReactiveTuple7[T1, T2, T3, T4, T5, T6, T7](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value))
  }

  class ReactiveTuple8[T1, T2, T3, T4, T5, T6, T7, T8](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value))
  }

  class ReactiveTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value))
  }

  class ReactiveTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value))
  }

  class ReactiveTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value))
  }

  class ReactiveTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value))
  }

  class ReactiveTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value))
  }

  class ReactiveTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value))
  }

  class ReactiveTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value))
  }

  class ReactiveTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value))
  }

  class ReactiveTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value))
  }

  class ReactiveTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value))
  }

  class ReactiveTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value))
  }

  class ReactiveTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value))
  }

  class ReactiveTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value, t._21.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value, t._21.value))
  }

  class ReactiveTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21], Reactive[T22])) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => V) = Reactive(Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value, t._21.value, t._22.value))

    @inline def apply[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => V) = (Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22).asInstanceOf[List[Reactive[V]]], (v: Seq[V]) => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value, t._21.value, t._22.value))
  }

}

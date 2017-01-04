package com.barbalion.lib.react

/**
  * Reactive Cell implementation. It can store one [[Reactive#value() value]] of type <code>T</code>.
  * The value can be dependant of other cells.
  * Use [[com.barbalion.lib.react.Reactive$ Reactive]] object to create new instances of the cells.
  * Assign new values (constant or reactive) to <code>value</code> property.
  *
  * @tparam T the type of cell's value
  * @param func       calc function of the cell
  * @param ps         producers to subscribe to recalculate the cell
  * @param calculator [[com.barbalion.lib.react.Calculator Calculator]] to run calculations
  * @see [[Reactive#value value]] property
  */
class Reactive[T](func: () => T, ps: Producer[_]*)(implicit val calculator: Calculator) extends Producer[T] with Consumer {
  def this(constant: T)(implicit calculator: Calculator) = this(() => constant)

  private var valid = true

  /**
    * The variable function that will calculate the result for us
    */
  protected var calcFunction = func

  /**
    * Cached last known value
    */
  protected var lastValue: T = func()

  /**
    * Set calculated value to the cell
    * Producers will trigger recalculation.
    *
    * @param v the value
    */
  def :=(v: () => T): Unit = {
    calcFunction = v
    unsubscribeFromAll()
    calculator.valueSet(this)
  }

  /**
    * Set constant value to the cell
    * No any trigger will affect this constant value.
    *
    * @param v the value
    */
  def :=(v: T): Unit = :=(() => v)

  /**
    * Set reactive value to the cell. It will automatically re-calculate if producer (i.e. other cell) changes.
    *
    * @param v Reactive value. Use syntax sugar to create to [[Reactive]]s.
    */
  //
  def :=(v: Reactive[T]): Unit = {
    :=(v.calcFunction)
    v.producers foreach (p => consume(p._1))
  }

  /**
    * Syntax sugar to spawn new dependent reactive cell
    *
    * @param f function to calculate result
    * @tparam V type of the result
    * @return new <code>Reactive[V]</code> object
    */
  @inline def >>[V](f: (T) => V) = new Reactive(() => f(value), this)

  /**
    * The produced value, auto-calculated if necessary
    *
    * @return current value of the cell
    */
  override def value: T = {
    if (!valid) calculator.valueFirstRead(this)
    lastValue
  }

  /**
    * Invalidate cell value and re-calculate it next time it's read.
    */
  def invalidate(): Unit = {
    if (valid) {
      valid = false
      consumers foreach {
        case r: Reactive[_] => r.invalidate()
        case _ =>
      }
    }
  }

  /**
    * Recalculates new value, trigger notification if it was changed
    */
  protected[react] def doCalc(): Unit = {
    calcFunction() match {
      case v if isDifferent(v, lastValue) => onValueChange(v)
      case _ =>
    }
    valid = true
  }

  /**
    * Called when the value is going to change. Assigns new value, trigger the notification
    *
    * @param newValue new value
    */
  protected def onValueChange(newValue: T): Unit = {
    lastValue = newValue
    notifyConsumers()
  }

  /**
    * Compares two values to decide if the value changed.
    *
    * @param newValue  new calculated value
    * @param lastValue previous know value
    * @return <code>true</code> if the values differ
    */
  protected def isDifferent(newValue: T, lastValue: T): Boolean = newValue != lastValue

  /**
    * Unsubscribe from all producers and keep last value, so no new recalculation of the value will occur.
    */
  @inline def unbind(): Unit = :=(value)

  override protected[react] def onProducerChange(p: Producer[_]): Unit = calculator.needReCalc(this)

  override def toString: String = value.toString

  {
    consume(ps)
  }
}

/**
  * Implicit conversions and syntax sugar object for [[Reactive Reactive]] class.
  * Use [[Reactive#>>(scala.Function1) >>]] to spawn new Reactive[T] objects.
  */
//noinspection LanguageFeature
object Reactive {
  /**
    * Creates [[Reactive Reactive]] cell object with specified initial value.
    *
    * @param func calc function of the cell
    * @param ps   producers to subscribe to recalculate the cell
    * @tparam T type of the cell value
    * @return new instance of Reactive object
    */
  def apply[T](func: () => T, ps: Producer[_]*)(implicit calculator: Calculator): Reactive[T] = new Reactive[T](func, ps: _*)(calculator)

  def apply[T](v: T)(implicit calculator: Calculator) = new Reactive[T](() => v)(calculator)

  implicit def reactTupleConv[T1, T2](t: (Reactive[T1], Reactive[T2]))(implicit calculator: Calculator): ReactiveTuple2[T1, T2] = new ReactiveTuple2(t)

  implicit def reactTupleConv[T1, T2, T3](t: (Reactive[T1], Reactive[T2], Reactive[T3]))(implicit calculator: Calculator): ReactiveTuple3[T1, T2, T3] = new ReactiveTuple3(t)

  implicit def reactTupleConv[T1, T2, T3, T4](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4]))(implicit calculator: Calculator): ReactiveTuple4[T1, T2, T3, T4] = new ReactiveTuple4(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5]))(implicit calculator: Calculator): ReactiveTuple5[T1, T2, T3, T4, T5] = new ReactiveTuple5(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6]))(implicit calculator: Calculator): ReactiveTuple6[T1, T2, T3, T4, T5, T6] = new ReactiveTuple6(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7]))(implicit calculator: Calculator): ReactiveTuple7[T1, T2, T3, T4, T5, T6, T7] = new ReactiveTuple7(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8]))(implicit calculator: Calculator): ReactiveTuple8[T1, T2, T3, T4, T5, T6, T7, T8] = new ReactiveTuple8(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9]))(implicit calculator: Calculator): ReactiveTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new ReactiveTuple9(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10]))(implicit calculator: Calculator): ReactiveTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new ReactiveTuple10(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11]))(implicit calculator: Calculator): ReactiveTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new ReactiveTuple11(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12]))(implicit calculator: Calculator): ReactiveTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new ReactiveTuple12(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13]))(implicit calculator: Calculator): ReactiveTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new ReactiveTuple13(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14]))(implicit calculator: Calculator): ReactiveTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new ReactiveTuple14(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15]))(implicit calculator: Calculator): ReactiveTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new ReactiveTuple15(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16]))(implicit calculator: Calculator): ReactiveTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new ReactiveTuple16(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17]))(implicit calculator: Calculator): ReactiveTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new ReactiveTuple17(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18]))(implicit calculator: Calculator): ReactiveTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new ReactiveTuple18(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19]))(implicit calculator: Calculator): ReactiveTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new ReactiveTuple19(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20]))(implicit calculator: Calculator): ReactiveTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new ReactiveTuple20(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21]))(implicit calculator: Calculator): ReactiveTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new ReactiveTuple21(t)

  implicit def reactTupleConv[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21], Reactive[T22]))(implicit calculator: Calculator): ReactiveTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new ReactiveTuple22(t)

  class ReactiveSeq[T](seq: TraversableOnce[Reactive[T]])(implicit calculator: Calculator) {
    @inline def >>[V](f: TraversableOnce[T] => V) = new Reactive(() => f(seq map (_.value)), seq.toSeq: _*)
  }

  implicit def reactSeqConv[T](seq: TraversableOnce[Reactive[T]])(implicit calculator: Calculator): ReactiveSeq[T] = new ReactiveSeq[T](seq)

  class ReactiveTuple2[T1, T2](t: (Reactive[T1], Reactive[T2]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2) => V) = Reactive(() => f(t._1.value, t._2.value), Seq(t._1, t._2): _*)
  }

  class ReactiveTuple3[T1, T2, T3](t: (Reactive[T1], Reactive[T2], Reactive[T3]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value), Seq(t._1, t._2, t._3): _*)
  }

  class ReactiveTuple4[T1, T2, T3, T4](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value), Seq(t._1, t._2, t._3, t._4): _*)
  }

  class ReactiveTuple5[T1, T2, T3, T4, T5](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value), Seq(t._1, t._2, t._3, t._4, t._5): _*)
  }

  class ReactiveTuple6[T1, T2, T3, T4, T5, T6](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6): _*)
  }

  class ReactiveTuple7[T1, T2, T3, T4, T5, T6, T7](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7): _*)
  }

  class ReactiveTuple8[T1, T2, T3, T4, T5, T6, T7, T8](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8): _*)
  }

  class ReactiveTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9): _*)
  }

  class ReactiveTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10): _*)
  }

  class ReactiveTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11): _*)
  }

  class ReactiveTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12): _*)
  }

  class ReactiveTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13): _*)
  }

  class ReactiveTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14): _*)
  }

  class ReactiveTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15): _*)
  }

  class ReactiveTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16): _*)
  }

  class ReactiveTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17): _*)
  }

  class ReactiveTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18): _*)
  }

  class ReactiveTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19): _*)
  }

  class ReactiveTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20): _*)
  }

  class ReactiveTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value, t._21.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21): _*)
  }

  class ReactiveTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (Reactive[T1], Reactive[T2], Reactive[T3], Reactive[T4], Reactive[T5], Reactive[T6], Reactive[T7], Reactive[T8], Reactive[T9], Reactive[T10], Reactive[T11], Reactive[T12], Reactive[T13], Reactive[T14], Reactive[T15], Reactive[T16], Reactive[T17], Reactive[T18], Reactive[T19], Reactive[T20], Reactive[T21], Reactive[T22]))(implicit calculator: Calculator) {
    @inline def >>[V](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => V) = Reactive(() => f(t._1.value, t._2.value, t._3.value, t._4.value, t._5.value, t._6.value, t._7.value, t._8.value, t._9.value, t._10.value, t._11.value, t._12.value, t._13.value, t._14.value, t._15.value, t._16.value, t._17.value, t._18.value, t._19.value, t._20.value, t._21.value, t._22.value), Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22): _*)
  }

}

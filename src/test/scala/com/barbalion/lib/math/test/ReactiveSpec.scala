package com.barbalion.lib.math.test

import com.barbalion.lib.react._
import org.scalatest.{FlatSpec, Matchers}

class ReactiveSpec extends FlatSpec with Matchers {
  implicit val smartCalculator = SmartCalculator
  val x = Reactive(1.5)
  "Static reactive value " must " keep its value" in {
    x.value should be(1.5)
    x.value = 0
    x.value should be(0)
  }

  val y = x >> ((x) => x + 1)
  "Dependent reactive value" must " calc init value" in {
    y.value should be(1)
  }

  "Dependent reactive value" must " re-calc new value" in {
    x.value = 1
    y.value should be(2)
  }

  val z = (x, y) >> ((x, y) => x * 2 + y * 3) // x = 1, y = 2 here
  "Multi-dependent reactive value" must " calc init value" in {
    z.value should be(8)
  }

  "Multi-dependent reactive value" must " re-calc new value on every change" in {
    x.value = 2 // y = 3 after this
    z.value should be(13) // z = 2x + 3y
    y.value = 5
    z.value should be(19)
  }

  "Syntax sugar " must " compile" in {
    z.value = x(x => x * 2.0)
    z.value should be(4)

    z.value = (x, y) ((x, y) => x * 2.0 + y)
    z.value should be(9)
  }

  "Hetero-type sugar " must " compile" in {
    val sqr = Reactive((a: Int) => a * a)
    val s = Reactive("foo")
    val s2 = (sqr, s, z) >> ((sqr, s, z) => s + sqr(z.toInt).toString)
    s2.value should be("foo81")
  }

  "Sequence " must " compile" in {
    val seq = Seq(Reactive(1), Reactive(2), Reactive(3)) >> (seq => seq.sum)
    seq.value should be(6)
  }

  "Consume usage model " must " calculate " in {
    var c1 = Reactive(0)
    val c2 = new Reactive[Int] {
      override protected def default: Int = 0
      value = () => {
        c1.value + 1
      }
      consume(c1)
    }
    c2.value should be(1)
    c1.value = 1
    c2.value should be(2)
  }

  "SmartCalculator" must "calculate circular dependencies" in {
    smartCalculator.done should be (true)
    val c1 = Reactive(0)
    val c2 = Reactive(0)
    c2.value = c1(c1 => c1 + 1)
    c1.value = c2(c2 => c2 + 1)
    c1.value should be(2)
    c2.value should be(3)
    smartCalculator.done should be (false)
//    smartCalculator.continue()
    c1.value should be(4)
    c2.value should be(5)
    smartCalculator.done should be (false)
  }

  "WaveCalculator" must "calculate circular dependencies" in {
    val wc = WaveCalculator
    wc.done should be (true)
    val c1 = Reactive(0)(wc)
    val c2 = Reactive(0)(wc)
    c2.value = c1(c1 => c1 + 1)
    c1.value = c2(c2 => c2 + 1)
    wc.continue()
    c1.value should be(2)
    c2.value should be(3)
    wc.done should be (false)
    wc.continue()
    c1.value should be(4)
    c2.value should be(3)
    wc.continue()
    c2.value should be(5)
    wc.done should be (false)
  }


}
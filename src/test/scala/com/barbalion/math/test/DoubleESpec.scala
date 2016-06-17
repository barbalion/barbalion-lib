package com.barbalion.math.test

import com.barbalion.math.DoubleE
import com.barbalion.math.DoubleE._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DoubleESpec extends FlatSpec {
  Zero.value == 0 && Zero.err == 0 && Zero == new DoubleE(0, 0, 0) should be(true)

  One.value == 1 && One.err == 0 && One == new DoubleE(1, 0, 0) should be(true)

  Zero != DoubleE(1, 0, 0) shouldBe true

  One + Two === Three shouldBe true

  Two * Two === Four shouldBe true
  Two * 2 === Four shouldBe true

  val x = DoubleE(1.5, 4, 0)
  val y = DoubleE(2.5, 9, 0)
  val list = List(
    DoubleE(1, 1, 0),
    DoubleE(2, 1, 0),
    DoubleE(3, 1, 0),
    DoubleE(4, 1, 0))

  x.err shouldBe 2
  x.err2 shouldBe 4

  "error2 of sum" must " be equal sum of error2s" in {
    assert((x + y).err2 == 13)
    assert((x + Err(6)).err2 == 10)
    assert((One + Two).err2 == 0)
  }

  withClue("mean") {
    val e: DoubleE = mean(list)
    e.value should be(2.5)
    e.err2 should be(1.25)
  }

  withClue("weightedMean") {
    val e: DoubleE = weightedMean(list)
    e.value should be(2.5)
    e.err2 should be(1.5)
  }

  withClue("Constant cache") {
    DoubleE.fromDouble(1) should be theSameInstanceAs One
  }

  (One + 1) shouldBe a[DoubleE]
  //  (1 + One) shouldBe a[DoubleE]

}
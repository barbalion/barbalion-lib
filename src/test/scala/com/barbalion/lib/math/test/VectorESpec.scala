package com.barbalion.lib.math.test

import com.barbalion.lib.math.{DoubleE, Vector2E, VectorE}
import org.scalatest.FlatSpec

class VectorESpec extends FlatSpec {
  "Vector combination" must "calculate" in {
    val v1 = Vector2E(1, 2)
    val v2 = Vector2E(3, 4)
    assert((v1+v2).x.value == 4)
    assert((v1+v2).y.value == 6)
    assert((v1-v2).x.value == -2)
    assert((v1-v2).y.value == -2)
  }

  "Vector reverse" must "calculate" in {
    val v1 = Vector2E(1, 2)
    val rv1: Vector2E = v1.reverse
    assert(rv1.x.value == -1)
    assert(rv1.y.value == -2)
  }

  "Vector pos" must "reflect" in {
    val v1 = Vector2E(1, 2)
    assert(v1.posE._1.value == 1)
    assert(v1.posE._2.value == 2)
    assert(v1.pos._1 == 1)
    assert(v1.pos._2 == 2)
    assert(v1.posFloat._1 == 1)
    assert(v1.posFloat._2 == 2)
  }

  "Vector length" must "calculate" in {
    val v1 = Vector2E(3, 4)
    assert(v1.r.value == 5)
  }

}
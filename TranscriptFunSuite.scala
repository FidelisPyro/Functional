

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import Grade._ 
val recordGen = for {
  course  <- Gen.alphaStr
  grade   <- Gen.oneOf(Grade.values.toList)
  credits <- Gen.choose(1, 4)
} yield Record(course, grade, credits)

implicit lazy val recordArbitrary : Arbitrary[Record] = Arbitrary(recordGen)


class TranscriptFunSuite extends ScalaCheckSuite
{
    test("simple gpaFun test"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4))
        val obtained = gpaFun(t)
        val expected:Double = 3.50
        assertEquals(obtained, expected)
    }

    test("gpaFun test with 0 credits"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 0), Record("cosc2030", Grade.B, 0))
        val obtained = gpaFun(t)
        val expected:Double = 0.0
        assertEquals(obtained, expected)
    }

    test("simple isHonorRollFun test"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4))
        val obtained = isHonorRollFun(t)
        val expected:Boolean = true
        assertEquals(obtained, expected)
    }

    test("isHonorRollFun test with a C"){
        val t:TranscriptData = List(Record("cosc1030", Grade.C, 4), Record("cosc2030", Grade.B, 4))
        val obtained = isHonorRollFun(t)
        val expected:Boolean = false
        assertEquals(obtained, expected)
    }

    test("simple addFun test"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4))
		val new_record = Record("cosc3020", Grade.C, 3)
        val obtained = addFun(t, new_record)
        val expected:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4), Record("cosc3020", Grade.C, 3))
        assertEquals(obtained, expected)
    }


/*
  property("cross product is almost commutative") {
    forAll { (x: Vec3, y: Vec3) =>
      val crosspro:Vec3 = cross(x,y)
      val negy:Vec3 = (-y(0), -y(1), -y(2))
      val crossproCom:Vec3 = cross(negy, x)
      assertEquals(crosspro._1, crossproCom._1)
      assertEquals(crosspro._2, crossproCom._2)
      assertEquals(crosspro._3, crossproCom._3)
    }
  }
  */
}

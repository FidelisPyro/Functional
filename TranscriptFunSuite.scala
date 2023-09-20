
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
    print("test print")
    test("simple gpaFun test")
    {
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4))
        val obtained = gpaFun(t)
        val expected:Double = 3.50
        assertEquals(obtained, expected)
    }
}

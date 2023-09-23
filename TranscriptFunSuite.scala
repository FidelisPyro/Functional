//Kyle Lofthus

package transcript

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import Grade._ 
val recordGen = for {
  course  <- Gen.alphaStr.suchThat(_.nonEmpty)
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
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4))
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

    test("simple containsFun passing test"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4), Record("cosc3020", Grade.F, 3))
        val obtained = containsFun(t, "cosc2030")
        val expected:Boolean = true
        assertEquals(obtained, expected)
    }

      test("simple containsFun failing test"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4), Record("cosc3020", Grade.F, 3))
        val obtained = containsFun(t, "cosc3020")
        val expected:Boolean = false
        assertEquals(obtained, expected)
    }

      test("containsFun test where the course isn't there"){
        val t:TranscriptData = List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4))
        val obtained = containsFun(t, "cosc3020")
        val expected:Boolean = false
        assertEquals(obtained, expected)
    }


    property("Adding a record increases the length of a transcript") {
    forAll { (t: TranscriptData, record: Record) =>
      val original_len: Int = t.length
      val t_added: TranscriptData = addFun(t, record)
      val new_len: Int = t_added.length
      assertEquals(new_len, (original_len + 1))
    }
  }

    property("Adding a passing record will mean the added course is contained in the new transcript") {
    forAll { (t: TranscriptData, record: Record) =>
      val t_added: TranscriptData = addFun(t, record)
      val obtained = containsFun(t_added, record.course_num)
      val passed: Boolean = record.grade.num_grade >= 2.0
      assertEquals(obtained, passed)
    }
  }

    property("The GPA is always between 0 and 4") {
    forAll { (t: TranscriptData) =>
      val gpa: Double = gpaFun(t)
      if gpa >= 0 & gpa <= 4
        then true
        else false
    }
  }

    property("The GPA increases (decreases) when you add a record with a higher (lower) grade than the old GPA") {
    forAll { (t: TranscriptData, record: Record) =>
      val old_gpa: Double = gpaFun(t)
      val t_added: TranscriptData = addFun(t, record)
      val new_gpa: Double = gpaFun(t_added) 
      if old_gpa >= record.grade.num_grade
        then old_gpa >= new_gpa
        else old_gpa <= new_gpa
    }
  }

}

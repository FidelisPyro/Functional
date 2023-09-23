//Kyle Lofthus

package transcript


import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

val transcriptGen = for {
  courses <- Arbitrary.arbitrary[List[Record]]
} yield Transcript(courses)

implicit lazy val transcriptArbitrary : Arbitrary[Transcript] = Arbitrary(transcriptGen)

class TranscriptSuite extends ScalaCheckSuite
{
    test("simple gpa test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4)))
        val obtained = t.gpa
        val expected:Double = 3.50
        assertEquals(obtained, expected)
    }

    test("gpa test with 0 credits"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 0), Record("cosc2030", Grade.B, 0)))
        val obtained = t.gpa
        val expected:Double = 0.0
        assertEquals(obtained, expected)
    }

    test("simple isHonorRoll test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4)))
        val obtained = t.isHonorRoll
        val expected:Boolean = true
        assertEquals(obtained, expected)
    }

    test("isHonorRoll test with a C"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4)))
        val obtained = t.isHonorRoll
        val expected:Boolean = false
        assertEquals(obtained, expected)
    }

    test("simple + test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4)))
        val new_record = Record("cosc3020", Grade.C, 3)
        val obtained = t+(new_record)
        val expected:Transcript = Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4), Record("cosc3020", Grade.C, 3)))
        assertEquals(obtained.records, expected.records)
    }

    test("simple contains passing test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4), Record("cosc3020", Grade.F, 3)))
        val obtained: Boolean = t.records.contains(Record("cosc1030", Grade.A, 4))
        assertEquals(obtained, true)
    }

      test("simple contains failing test"){
        val t:Transcript = new Transcript(List(Record("cosc2030", Grade.C, 4), Record("cosc3020", Grade.F, 3)))
        val obtained = t.records.contains(Record("cosc1030", Grade.A, 4))
        assertEquals(obtained, false)
    }

    property("Adding a record increases the length of a transcript") {
    forAll { (t: Transcript, record: Record) =>
      val original_len: Int = t.records.length
      val t_added: Transcript = t+(record)
      val new_len: Int = t_added.records.length
      assertEquals(new_len, (original_len + 1))
    }
  }

    property("Adding a passing record will mean the added course is contained in the new transcript") {
    forAll { (t: Transcript, record: Record) =>
      val t_added: Transcript = t+(record)
      val obtained = t_added.contains(record)
      val passed: Boolean = record.grade.num_grade >= 2.0
      assertEquals(obtained, passed)
    }
  }

}

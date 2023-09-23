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
    //testing gpa
    test("simple gpa test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4)))
        val obtained = t.gpa
        val expected:Double = 3.50
        assertEquals(obtained, expected)
    }

    //testing gpa with 0 credits 
    test("gpa test with 0 credits"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 0), Record("cosc2030", Grade.B, 0)))
        val obtained = t.gpa
        val expected:Double = 0.0
        assertEquals(obtained, expected)
    }

    //testing isHonorRoll
    test("simple isHonorRoll test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4)))
        val obtained = t.isHonorRoll
        val expected:Boolean = true
        assertEquals(obtained, expected)
    }

    //testing isHonorRoll with a C to provoke a false case
    test("isHonorRoll test with a C"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4)))
        val obtained = t.isHonorRoll
        val expected:Boolean = false
        assertEquals(obtained, expected)
    }

    //+ test
    test("simple + test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4)))
        val new_record = Record("cosc3020", Grade.C, 3)
        val obtained = t+(new_record)
        val expected:Transcript = Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.B, 4), Record("cosc3020", Grade.C, 3)))
        assertEquals(obtained.records, expected.records)
    }

    //contains test where passing is true
    test("simple contains passing test"){
        val t:Transcript = new Transcript(List(Record("cosc1030", Grade.A, 4), Record("cosc2030", Grade.C, 4), Record("cosc3020", Grade.F, 3)))
        val obtained: Boolean = t.records.contains(Record("cosc1030", Grade.A, 4))
        assertEquals(obtained, true)
    }

      //contains test where passing is false
      test("simple contains failing test"){
        val t:Transcript = new Transcript(List(Record("cosc2030", Grade.C, 4), Record("cosc3020", Grade.F, 3)))
        val obtained = t.records.contains(Record("cosc1030", Grade.A, 4))
        assertEquals(obtained, false)
    }

    //property to prove that adding a record increases the length of the transcript
    property("Adding a record increases the length of a transcript") {
    forAll { (t: Transcript, record: Record) =>
      val original_len: Int = t.records.length
      val t_added: Transcript = t+(record)
      val new_len: Int = t_added.records.length
      assertEquals(new_len, (original_len + 1))
    }
  }

    //property to prove that adding a passing will add the course in the new transcript
    property("Adding a passing record will mean the added course is contained in the new transcript") {
    forAll { (t: Transcript, record: Record) =>
      val t_added: Transcript = t+(record)
      val obtained = t_added.contains(record)
      val passed: Boolean = record.grade.num_grade >= 2.0
      assertEquals(obtained, passed)
    }
  }

    //property to prove that GPA is always between 0 and 4
    property("The GPA is always between 0 and 4") {
    forAll { (t: Transcript) =>
      val gpa_score: Double = new Transcript(t.records).gpa
      if gpa_score >= 0 & gpa_score <= 4
        then true
        else false
    }
  }

    //property to prove that if add a course that is lower/higher than the GPA then the gpa will go down/up respectively
    property("The GPA increases (decreases) when you add a record with a higher (lower) grade than the old GPA") {
    forAll { (t: Transcript, record: Record) =>
      val old_gpa: Double = new Transcript(t.records).gpa 
      val t_added: Transcript = t+(record)
      val new_gpa: Double = new Transcript(t_added.records).gpa 
      if old_gpa >= record.grade.num_grade
        then old_gpa >= new_gpa
        else old_gpa <= new_gpa
    }
  }

}

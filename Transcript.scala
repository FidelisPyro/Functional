//Kyle Lofthus

package transcript


final class Transcript(val records: TranscriptData) 
	{
		def gpa: Double =
			{
				def getGPA(records: TranscriptData, gpaCredit: Double, credit_total: Int): Double =
					{
						records match
							case Nil if credit_total == 0 => 0.0
							case Nil => gpaCredit / credit_total
							case head :: tail => val class_gpaCredit: Double = head.grade.num_grade * head.credits
								getGPA(tail, gpaCredit + class_gpaCredit, credit_total + head.credits)
					}
				getGPA(records, 0.0, 0)
			}

		def isHonorRoll: Boolean =
			{
				def checkHonorRoll(records: TranscriptData): Boolean =
					{
						records match
							case Nil => true
							case head :: tail =>
								if head.grade.num_grade >= 3.0
								then checkHonorRoll(tail)
								else false
					}
				checkHonorRoll(records)
			}
		
		def +(r: Record): Transcript =
			{
				val new_transcript: Transcript = Transcript(records ::: List(r))
				new_transcript
			}

		def contains(r: Record): Boolean =
			{
				def doesItContain(transcript: TranscriptData, course: String): Boolean =
					{
						transcript match
							case head :: tail => if head.course_num == course 
								then head.grade.num_grade >= 2.0 
								else doesItContain(tail, course)
							case Nil => false
					}
				doesItContain(records, r.course_num)
			}
	}

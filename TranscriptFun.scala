
//Kyle Lofthus


enum Grade(val letter: String, val num_grade: Double):
    case A extends Grade("A", 4.0)
    case B extends Grade("B", 3.0)
    case C extends Grade("C", 2.0)
    case D extends Grade("D", 1.0)
    case F extends Grade("F", 0.0)
end Grade


case class Record(course_num: String, grade: Grade, credits: Int)

type TranscriptData = List[Record]


def gpaFun(records: TranscriptData): Double = 
    {
		def calculateGPA(records: TranscriptData, gpaCredit: Double, credit_total: Int): Double =
			{
				records match 
					case Nil if(credit_total == 0) => 0.0 
					case Nil => gpaCredit / credit_total
					case head :: tail => val class_gpaCredit: Double = head.grade.num_grade * head.credits
						calculateGPA(tail, gpaCredit + class_gpaCredit, credit_total + head.credits)
			}
		calculateGPA(records, 0.0, 0)
	}

def isHonorRollFun(records: TranscriptData): Boolean =
	{
		records match
			case Nil => true 
			case head :: tail => 
				if head.grade.num_grade >= 3.0
				then isHonorRollFun(tail) 
				else false
	}

def addFun(transcript: TranscriptData, record:Record): TranscriptData =
	{
		transcript match
			case head :: tail => if head.course_num == record.course_num then transcript
				else addFun(tail, record)
			case Nil => val new_transcript: TranscriptData = transcript ::: List(record)
	}

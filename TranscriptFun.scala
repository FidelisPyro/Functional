
//Kyle Lofthus

//creates Grade that holds letter grades, and their equivalent gpa number
enum Grade(val letter: String, val num_grade: Double):
    case A extends Grade("A", 4.0)
    case B extends Grade("B", 3.0)
    case C extends Grade("C", 2.0)
    case D extends Grade("D", 1.0)
    case F extends Grade("F", 0.0)
end Grade

//creates the case class Records to hold the course, grade, and credits
case class Record(course_num: String, grade: Grade, credits: Int)

//creates alias of a list of records called TranscriptData
type TranscriptData = List[Record]

//recursively goes through the TranscriptData to calculate the weighted gpa
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

//recursively goes through the TranscriptData and if any grade is < B then returns false
//this assumes there is no empty list, because checking if a student who has not taken
//any classes is on the honor roll, doesn't make since.
def isHonorRollFun(records: TranscriptData): Boolean =
	{
		records match
			case Nil => true 
			case head :: tail => 
				if head.grade.num_grade >= 3.0
				then isHonorRollFun(tail) 
				else false
	}

//recursively goes through the TranscriptData and if any course number matches the
// the new record to be added, then that course is already there and no need to add it.
//If I have time before due date, I will add that if they match the new one replaces
//the old one (student retook class for a "hopefully better grade")
def addFun(transcript: TranscriptData, record:Record): TranscriptData =
	{
		transcript match
			case head :: tail => if head.course_num == record.course_num then transcript
				else head :: addFun(tail, record)
			case Nil => transcript ::: List(record)
	}


//recursively goes through the TranscriptData and if it finds the course we're looking
//for it checks if the grade for that course is a C or better (passing) and returns
//true if it is and false if it isn't. It also returns false if the course isn't found.
//Can't pass a course you haven't taken.
def containsFun(transcript: TranscriptData, course: String): Boolean = 
    {
        transcript match
            case head :: tail => if head.course_num == course 
                then if head.grade.num_grade >= 2.0 then true else false
                else containsFun(tail, course)
            case Nil => false
    }

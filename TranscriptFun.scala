//Kyle Lofthus


enum Grade(val letter: String, val gpa: Double):
    case A extends Grade("A", 4.0)
    case B extends Grade("B", 3.0)
    case C extends Grade("C", 2.0)
    case D extends Grade("D", 1.0)
    case F extends Grade("F", 0.0)
end Grade

/*enum Grade(val letter: String):
    case A extends Grade("A")
    case B extends Grade("B")
    case C extends Grade("C")
    case D extends Grade("D")
    case F extends Grade("F")
*/

case class Record(val course_num: String, grade: Grade, num_credits: Int)

type TranscriptData = List[Record]


def gpaFun(records: TranscriptData): Double = 
    {
        var credit_total = 0 
        var weighted_sum = 0.0

        for(record <- records) 
        {
            val weighted_gpa = record.grade.gpa * record.num_credits
            weighted_sum += weighted_gpa
            credit_total += record.num_credits
        }

        if(credit_total > 0)
        {
            weighted_sum / credit_total
        }
        else
        {
            0.0
        }
    }

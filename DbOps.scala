//Kyle Lofthus

package base	

case class Employee(ID: Int, Name: String, Age: Int, Salary: Double)
case class Department(dept_ID: Int, Name: String, Location: String)
case class EmployeeDepartment(e_ID: Int, d_ID: Int)

type Table = List[Tuple]

val emps = List(
    Tuple.fromProductTyped(Employee(1, "Barney", 30, 100000.0)),
    Tuple.fromProductTyped(Employee(2, "Betty", 20, 125000.0)),
    Tuple.fromProductTyped(Employee(3, "Wilma", 25, 95000.0)),
    Tuple.fromProductTyped(Employee(4, "Fred", 25, 90000.0)),
)

val depts = List(
    Tuple.fromProductTyped(Department(1, "Quarry", "Bedrock")),
    Tuple.fromProductTyped(Department(2, "Transportation", "Laramie")),
    Tuple.fromProductTyped(Department(3, "Sales", "Denver")),
    Tuple.fromProductTyped(Department(4, "Abacus", "Austin")),
)

val worksIn = List(
    Tuple.fromProductTyped(EmployeeDepartment(1, 1)),
    Tuple.fromProductTyped(EmployeeDepartment(1, 2)),
    Tuple.fromProductTyped(EmployeeDepartment(2, 2)),
    Tuple.fromProductTyped(EmployeeDepartment(2, 3)),
    Tuple.fromProductTyped(EmployeeDepartment(3, 3)),
    Tuple.fromProductTyped(EmployeeDepartment(3, 4)),
    Tuple.fromProductTyped(EmployeeDepartment(4, 4)),
    Tuple.fromProductTyped(EmployeeDepartment(4, 1)),
)


def project(table: Table, colPred: (Int) => Boolean) : Table = 
	def projRows(row: List[Int], colPred: Int => Boolean, newRows: List[A], index: Int): List[Any] =
		row match
			case Nil => newRows
			case h::t => if colPred(index) then projRows(t, colPred, newRows :: h, index + 1)
				else projRows(t, colPred, newRows, index + 1)
	table match 
		case Nil => Nil
		case h::t =>
			projRows(h, colPred, List[], 0)

	


//def select(table: Table, rowPred: (Tuple) => Boolean) : Table = ???
//def join(lhs: Table, rhs: Table, theta: (Tuple, Tuple) => Boolean) : Table = ???





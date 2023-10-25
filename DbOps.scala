
//Kyle Lofthus

package base	

case class Employee(ID: Int, Name: String, Age: Int, Salary: Double)
case class Department(dept_ID: Int, Name: String, Location: String)
case class EmployeeDepartment(e_ID: Int, d_ID: Int)



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



type Table = List[Tuple]

def project(table: Table, colPred: (Int) => Boolean) : Table = {
    def projRows(row: Tuple, colPred: Int => Boolean, newRows: Tuple, index: Int): Tuple = {
        row match {
            case EmptyTuple => newRows
            case h*:t => 
                if colPred(index) then projRows(t, colPred, newRows ++ Tuple(h), index + 1)
                else projRows(t, colPred, newRows, index + 1)
        }
    }

    table match { 
        case Nil => Nil
        case h::t =>
            projRows(h, colPred, EmptyTuple, 0) :: project(t, colPred)
        }
}

def projectCols(list:List[Int]): (Int) => Boolean = {
    list match {
        case Nil => (x:Int) => false
        case h::t => (x:Int) => if x == h then true else projectCols(t)(x)
    }
}



def select(table: Table, rowPred: (Tuple) => Boolean) : Table = {
    def newTable(table: Table, rows: Table): Table = {
        table match {
            case Nil => rows.reverse
            case h::t => 
                if rowPred(h) then newTable(t, h :: rows)
                else newTable(t, rows)
        }
    }
    newTable(table, Nil)
}

def selectRows[A](idx:Int, pred: (A) => Boolean): (Tuple) => Boolean = {
    def checkRows[A](idx: Int, pred:(A) => Boolean, row: Tuple): Boolean = {
        def checkElements[A](idx: Int, row: Tuple): Option[A] = {
            row match {
                case EmptyTuple => None
                case h*:t => 
                    if idx == 0 then Some(h.asInstanceOf[A])
                    else checkElements(idx - 1, t)
            }
        }
        checkElements(idx, row) match {
            case None => false
            case Some(a) => pred(a)
        }
    }
    (row: Tuple) => checkRows(idx, pred, row)
}




//def join(lhs: Table, rhs: Table, theta: (Tuple, Tuple) => Boolean) : Table = ???





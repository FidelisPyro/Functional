
//Kyle Lofthus

package base	

case class Employee(emp_ID: Int, Name: String, Age: Int, Salary: Double)
case class Department(dept_ID: Int, Name: String, Location: String)
case class EmployeeDepartment(emp_ID: Int, dept_ID: Int)



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
    def newTable(table: Table, addedRows: Table): Table = {
        table match {
            case Nil => addedRows
            case h::t => 
                if rowPred(h) then newTable(t, h :: addedRows)
                else newTable(t, addedRows)
        }
    }
    newTable(table, Nil)
}

def selectRows[A](idx:Int, pred: (A) => Boolean): (Tuple) => Boolean = {
    def checkRows[A](idx: Int, pred: (A) => Boolean, row: Tuple): Boolean = {
        def checkElements[B](idx: Int, row: Tuple): Boolean = {
            row match {
                case EmptyTuple => false
                case h*:t => 
                    if idx == 0 && pred(h.asInstanceOf[A]) 
                    then if checkElements(idx - 1, t) then false else true
                    else checkElements(idx - 1, t)
            }
        }
        checkElements(idx, row) 
    }
    row => checkRows(idx, pred, row)
}


def join(lhs: Table, rhs: Table, theta: (Tuple, Tuple) => Boolean) : Table = {
    def joinRows(lhsRow: Tuple, rhs: Table, acc: Table): Table = {
        rhs match {
            case Nil => Nil
            case h::t => 
                val addedRow = lhsRow ++ h
                if theta(lhsRow, h) then addedRow :: joinRows(lhsRow, t, acc)
                else joinRows(lhsRow, t, acc)
        }
    }

    def joinTables(lhs: Table, rhs: Table, newTable: Table): Table = {
      lhs match {
        case Nil => newTable
        case h::t => 
          val joinedRows = joinRows(h, rhs, Nil)
          joinTables(t, rhs, newTable ++ joinedRows)
      }
    }
    joinTables(lhs, rhs, Nil)
}

def equiJoin[A](idx1:Int, idx2:Int): (Tuple, Tuple) => Boolean = {
    def checkElements(lhs: Tuple, rhs: Tuple): Boolean = {
        (lhs, rhs) match {
            case (EmptyTuple, EmptyTuple) => true
            case ((h1) *: t1, (h2) *: t2) => 
                if (idx1 == 0 && idx2 == 0 && h1 == h2) 
                then if checkElements(t1, t2) then false else true
                else checkElements(t1, t2)
            case _ => false
        }
    }
    (row1: Tuple, row2: Tuple) => checkElements(row1, row2) 
}

def andConds(preds: List[(Tuple) => Boolean]): (Tuple) => Boolean = {
    (row: Tuple) => {
        def checkPreds(preds: List[(Tuple) => Boolean]): Boolean = {
            preds match {
                case Nil => true
                case h::t => 
                    if h(row) then checkPreds(t)
                    else false
            }
        }
        checkPreds(preds)
    }
}

def orConds(preds: List[(Tuple) => Boolean]): (Tuple) => Boolean = {
    (row: Tuple) => {
        def checkPreds(preds: List[(Tuple) => Boolean]): Boolean = {
            preds match {
                case Nil => false
                case h::t => 
                    if h(row) then true
                    else checkPreds(t)
            }
        }
        checkPreds(preds)
    }
}


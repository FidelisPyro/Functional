//Kyle Lofthus

package base

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
//import DbOps._


class DbOpsSuite extends ScalaCheckSuite {
    
    test("Testing project with a single column") {
        val colPred: Int => Boolean = (index: Int) => index == 1
        val obtained = project(emps, colPred)

        val expected: Table = 
            List(
                Tuple("Barney"), 
                Tuple("Betty"), 
                Tuple("Wilma"), 
                Tuple("Fred"))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }


    test("Testing project with two columns") {
        val colPred: Int => Boolean = (index: Int) => index == 1 || index == 2
        val obtained = project(depts, colPred)

        val expected: Table = 
            List(
                ("Quarry", "Bedrock"), 
                ("Transportation", "Laramie"), 
                ("Sales", "Denver"), 
                ("Abacus", "Austin"))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing project with three columns") {
        val colPred: Int => Boolean = (index: Int) => index == 0 || index == 1 || index == 3
        val obtained = project(emps, colPred)

        val expected: Table = 
            List(
                (1, "Barney", 100000.0), 
                (2, "Betty", 125000.0), 
                (3, "Wilma", 95000.0), 
                (4, "Fred", 90000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }
    
    test("Testing projectCols with a single column") {
        //val listOfColumns = projectCols(List(1))
        val obtained = project(worksIn, projectCols(List(1)))

        val expected: Table = 
            List(
                Tuple(1),
                Tuple(2),
                Tuple(2), 
                Tuple(3),
                Tuple(3),
                Tuple(4),
                Tuple(4),
                Tuple(1))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing projectCols with three columns") {
        val listOfColumns = projectCols(List(0, 1, 3))
        val obtained = project(emps, listOfColumns)

        val expected: Table = 
            List(
                (1, "Barney", 100000.0), 
                (2, "Betty", 125000.0), 
                (3, "Wilma", 95000.0), 
                (4, "Fred", 90000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing select with an equality predicate") {
        
        val obtained = select(emps, selectRows(2, (Age: Int) => Age == 25))

        val expected: Table = 
            List((4,"Fred", 25, 90000.0),
                 (3,"Wilma", 25, 95000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing select with a string match predicate") {
        
        val obtained = select(depts, selectRows(2, (Name: String) => Name == "Laramie"))

        val expected: Table = 
            List((2,"Transportation", "Laramie"))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing select with a partial string match predicate") {
        
        val obtained = select(emps, selectRows(1, (Name: String) => Name.startsWith("B")))


        val expected: Table = 
            List((2,"Betty", 20, 125000.0),
                 (1,"Barney", 30, 100000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing join(theta)") {
        val obtained = join(emps, worksIn, (emp: Tuple, work: Tuple) => emp.productElement(0) == work.productElement(0))

        val expected: Table =
            List((1,"Barney",30,100000.0,1,1),
                 (1,"Barney",30,100000.0,1,2), 
                 (2,"Betty",20,125000.0,2,2), 
                 (2,"Betty",20,125000.0,2,3), 
                 (3,"Wilma",25,95000.0,3,3), 
                 (3,"Wilma",25,95000.0,3,4), 
                 (4,"Fred",25,90000.0,4,4), 
                 (4,"Fred",25,90000.0,4,1))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing equiJoin") {
        val obtained = join(emps, worksIn, equiJoin(0, 0)) 

        val expected: Table =
            List((1,"Barney",30,100000.0,1,1),
                 (1,"Barney",30,100000.0,1,2), 
                 (2,"Betty",20,125000.0,2,2), 
                 (2,"Betty",20,125000.0,2,3), 
                 (3,"Wilma",25,95000.0,3,3), 
                 (3,"Wilma",25,95000.0,3,4), 
                 (4,"Fred",25,90000.0,4,4), 
                 (4,"Fred",25,90000.0,4,1))

        assertEquals(obtained, expected)                 

        println("\n" + obtained + "\n")
    }

    test("Testing andConds") {
        val preds = List(
            (t: Tuple) => t.productElement(0).asInstanceOf[Int] > 0,
            (t: Tuple) => t.productElement(1).asInstanceOf[Any].toString.startsWith("B")
        )

        val obtained = select(emps, andConds(preds))
        val expected: Table = 
            List((2,"Betty",20,125000.0),                 
                 (1,"Barney",30,100000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing andConds2") {
        val preds = List(
            (t: Tuple) => t.productElement(2).asInstanceOf[Int] < 30,
            (t: Tuple) => t.productElement(3).asInstanceOf[Double] > 90000.0
        )

        val obtained = select(emps, andConds(preds))
        val expected: Table =
            List((3, "Wilma", 25, 95000.0),
                 (2, "Betty", 20, 125000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing andConds3") {
        val preds = List(
            (t: Tuple) => t.productElement(0).asInstanceOf[Int] > 1,
            (t: Tuple) => t.productElement(1).asInstanceOf[Int] < 4
        )

        val obtained = select(worksIn, andConds(preds))
        val expected: Table =
            List((4, 1),
                 (3, 3),
                 (2, 3),
                 (2, 2))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing orConds") {
        val preds = List(
            (t: Tuple) => t.productElement(1).asInstanceOf[Any].toString.startsWith("B"),
            (t: Tuple) => t.productElement(3).asInstanceOf[Double] > 90000.0
        )

        val obtained = select(emps, orConds(preds))
        val expected: Table =
            List((3, "Wilma", 25, 95000.0),
                 (2, "Betty", 20, 125000.0),
                 (1, "Barney", 30, 100000.0))
        
        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing orConds2") {
        val preds = List(
            (t: Tuple) => t.productElement(2).asInstanceOf[String] == "Laramie",
            (t: Tuple) => t.productElement(2).asInstanceOf[String] == "Denver"
        )

        val obtained = select(depts, orConds(preds))
        val expected: Table =
            List((3, "Sales", "Denver"),
                 (2, "Transportation", "Laramie" ))
        
        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Name and age of those who make >= 100000.0") {
        val colPred: Int => Boolean = (index: Int) => index == 1 || index == 2

        val filteredEmps = select(emps, selectRows(3, (Salary: Double) => Salary >= 100000.0))
        val obtained = project(filteredEmps, colPred)

        val expected: Table =
            List(("Betty", 20),
                 ("Barney", 30))
    
        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Name and salary of those who make > 25") {
        val colPred: Int => Boolean = (index: Int) => index == 1 || index == 3

        val filteredEmps = select(emps, selectRows(2, (Age: Int) => Age > 25))
        val obtained = project(filteredEmps, colPred)

        val expected: Table =
            List(("Barney", 100000.0))
    
        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Employee Name and their Department") {
        val joined = join(emps, worksIn, equiJoin(0, 0))

        val obtained = project(joined, projectCols(List(1, 5)))

        val expected: Table =
            List(("Barney", 1),
                 ("Barney", 2),
                 ("Betty", 2),
                 ("Betty", 3),
                 ("Wilma", 3),
                 ("Wilma", 4),
                 ("Fred", 4),
                 ("Fred", 1))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Employee List and Department List") {
        val joined = join(emps, depts, equiJoin(0, 0))

        val obtained = project(joined, projectCols(List(1, 5)))

        val expected: Table =
            List(("Barney", "Quarry"),
                 ("Betty", "Transportation"),
                 ("Wilma", "Sales"),
                 ("Fred", "Abacus"))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Employee Name their Department") {
        val joined = join(emps, worksIn, equiJoin(0, 0))
        val joined_trim = project(joined, projectCols(List(1, 5)))
        val joined2 = join(joined_trim, depts, equiJoin(1, 0))

        val joined2_test = join(joined, depts, (emp_dep: Tuple, deps: Tuple) => emp_dep.productElement(1) == deps.productElement(0))

        //val obtained = project(joined2, projectCols(List(1, 5)))

        val expected: Table =
            List(("Barney", "Quarry"),
                 ("Barney", "Transportation"),
                 ("Betty", "Transportation"),
                 ("Betty", "Sales"),
                 ("Wilma", "Sales"),
                 ("Wilma", "Abacus"),
                 ("Fred", "Abacus"),
                 ("Fred", "Quarry"))

        println("\nJoined: " + joined + "\n")
        println("\nJoined_trim: " + joined_trim + "\n")
        println("\nJoined2: " + joined2 + "\n")
        println("\nJoined2_test: " + joined2_test + "\n")
        //println("\nObtained: " + obtained + "\n")

        //assertEquals(obtained, expected)

        //println("\n" + joined + "\n")
        //println("\n" + joined2 + "\n")
        //println("\n" + obtained + "\n")
    }
}




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

    test("Testing select with something in index 0") {
        //val rowPred: Tuple => Boolean = selectRows(2, (age: Int) => age > 25)
        
        val obtained = select(emps, selectRows(2, (age: Int) => age == 25))

        val expected: Table = 
            List((3,"Wilma", 25, 95000.0),
                 (4,"Fred", 25, 90000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }

    test("Testing select with something in index 0") {
        //val rowPred: Tuple => Boolean = selectRows(2, (age: Int) => age > 25)
        
        val obtained = select(emps, selectRows(1, (name: String) => name.substring(0, 1) == "B"))

        val expected: Table = 
            List((1,"Barney", 30, 100000.0),
                 (2,"Betty", 20, 125000.0))

        assertEquals(obtained, expected)

        println("\n" + obtained + "\n")
    }
}


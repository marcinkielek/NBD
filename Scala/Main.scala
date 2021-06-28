import scala.annotation.tailrec
import scala.collection.convert.ImplicitConversions.`seq AsJavaList`
import scala.util.control.Breaks.{break, breakable}
//task_scala
object Main {
  def main(args: Array[String]) = {
       val week = List("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")

    println("task 1a")
    task1a(week)

    println("task 1b")
    println(task1b(week))

    println("task 1c")
    println(task1c(week))

    println("task 2a")
    println(task2a(week))

    println("task 2b")
    println(task2b(week))


    println("task 3")
    println(task3(week))



    println("task 4a")
    println(task4a(week))

    println("task 4b")
    println(task4b(week))

    println("task 4c")
    println(task4c(week))

    println("task 5")
    val products = Map("Banany" -> 1, "Bananowy Chleb" -> 3, "Sok Bananowy" -> 5)
    val da_products = products map { case (key, value) => (key, value * 1.1) }
    println(da_products)

    println("task 6")
    task6("banany", 4, week)


    println("task 7")

    val BANANA_PRICE = products.get("Banany")
    println(BANANA_PRICE)

    val BANANA_JUICE_PRICE = products.get("Sok Bananowy")
    println(BANANA_JUICE_PRICE)


    println("task 8")
    val intList = List(0, 0, 2, -1, 8, 0, 0, 1, 4, 0)
    println(task8(intList))



    println("task 9")
    println(task9(intList))

    println("task 10")
    val biggerIntList = List(-6, -5, -33, 2, 10, 1, 12, 3, 34, 5, 6, 7, 8, 9, 300)
    println(task10(biggerIntList))
  }

  def task1a(week: List[String]) = {
    var combinedString = "";
    for (i <- 0 until week.length) {
      if (combinedString != "")
        combinedString += ", " + week.get(i)
      else
        combinedString += week.get(i)
    }
    println(combinedString)

  }

  def task1b(week: List[String]) = {
    var combinedString = "";
    for (i <- 0 until week.length) {
      breakable {
        if (!week.get(i).startsWith("P")) break
        if (combinedString != "")
          combinedString += ", " + week.get(i)
        else
          combinedString += week.get(i)
      }


    }
    combinedString;
  }

  def task1c(week: List[String]) = {
    var combinedString = ""
    var index = 0;
    while (index < week.length) {
      if (combinedString != "")
        combinedString += ", " + week.get(index)
      else
        combinedString += week.get(index)

      index = index + 1;
    }

    combinedString
  }

  def task2a(week: List[String]) = {
    def addAnotherDay(i: Int): String = {
      if (i == week.length) return ""

      val currentDay = if (i == week.length - 1) week.get(i) else week.get(i) + ", "

      currentDay + addAnotherDay(i + 1)
    }

    addAnotherDay(0);
  }

  def task2b(week: List[String]): String = {
    def addAnotherDay(i: Int): String = {
      if (i == -1) return ""

      val currentDay = if (i == 0) week.get(i) else week.get(i) + ", "

      currentDay + addAnotherDay(i - 1)
    }

    addAnotherDay(week.length - 1)
  }

  def task3(week: List[String]) = {

    def addAnotherDay(i: Int, combinedString: String) : String = {
      if (i == week.length) return combinedString

      val currentDay = if (i == week.length - 1) week.get(i) else week.get(i) + ", "

      addAnotherDay(i + 1, combinedString + currentDay)
    }
    addAnotherDay(0, "")
  }

  def task4a(week: List[String]) : String = {
    week.fold("") {(sum, curr) => {
      sum + curr + ", "
    }}.dropRight(2)
  }

  def task4b(week: List[String]) : String = {
    week.foldRight("") {(sum, curr) => {
      sum + ", " + curr
    }}.dropRight(2)
  }

  def task4c(week: List[String]) : String = {
    week.fold("") {(sum, curr) => {
      if (curr.startsWith("P"))
        sum + curr + ", "
      else sum

    }}.dropRight(2)
  }

  def task6(tup: (String, Int, Any)) = {
    println(tup._1)
    println(tup._2)
    println(tup._3)
  }

  def task8(intList: List[Int]): List[Int] = {
    def iter(index: Int, currentList: List[Int]): List[Int] = {
      if (index >= currentList.length) return currentList;

      val (part1, part2) = currentList.splitAt(index)

      if (currentList.get(index) == 0)
        iter(index + 1, part1 ++ part2.tail)
      else
        iter(index + 1, currentList)
    }


    iter(0, intList)
  }

  def task9(intList: List[Int]): List[Int] = {
    intList map (el => el + 1)
  }

  def task10(intList: List[Int]): List[Int] = {
    val filteredList = intList filter (el => el >= -5 && el <= 12)

    filteredList map (el => el.abs)
  }


// ZADANIA Z DRUGIEJ CZĘŚCI!
//zadanie 1
def fun(day: String): String = day match {
  case "Sobota" | "Niedziela"                                      => "Weekend"
  case "Poniedziałek" | "Wtorek" | "Środa" | "Czwartek" | "Piątek" => "Praca"
  case _                                                           => "Nie ma takiego dnia"
}
println(
  "Poniedziałek oznacza " + fun("Poniedziałek")
)
println(
  "Sobota to " + fun("Sobota")
)
println(
  "Polska wygrywa Euro " + fun("Polska wygrywa Euro")
)
//zadanie 2
class KontoBankowe(private var balance: Int = 0) {
  def wplata(change: Int): Unit = {
    balance += change
  }
  def wyplata(change: Int): Unit = {
    balance -= change
  }
  def stanKonta(): Int = {
    balance
  }
}
val Konto = new KontoBankowe
Konto.wplata(100)
Konto.wyplata(10)

println("Stan pierwszego konta " + Konto.stanKonta())

//zadanie 3
case class Osoba(var name: String, var surname: String)
val ja = new Osoba("Marcin", "Kielek")
val ona = new Osoba("Klaudia", "Łuszkiewicz")
val kotjeden_traktujmy_jako_domyslnie = new Osoba("Bletka", "Null")
val kotdrugi = new Osoba("Bandzior", "Dziorek")
def elko(someone: Osoba): String = someone match {
  case Osoba("Marcin", "Kielek") => s"elo byku"
  case Osoba("Klaudia", "Łuszkiewicz")  => s"cześć klaudynka"
  case Osoba(name, "Dziorek") =>
    s"nie wiem czy chodzi o to w tym ćwiczeniu $name"
  case _                              => s"hejka Blecik"
}
println(elko(ja))
println(elko(ona))
println(elko(kotjeden_traktujmy_jako_domyslnie)) //zapomnialem, ze jedno mialo byc domyslnie, a nie moge faworyzowac kotow
println(elko(kotdrugi))
//zadanie 4
def task4(number: Int, fun: (Int) => Int): Int = {
  fun(fun(fun(number)))

}
  val number = 32
  def minusone(num: Int): Int = num - 1
  println(task4(number, minusone))
//zadanie 5
case class Osoba1(val imie: String, val nazwisko: String) {
  val podatek = 0
}
  trait Pracownik extends Osoba1 {
    override val podatek: Int = 20
    var pensja: Double = 0
  }
  trait Nauczyciel extends Pracownik {
  override val podatek: Int = 10
}
  trait Student extends Osoba1 {
    override val podatek: Int = 0
  }
  val osoba = new Osoba1("A", "A") with Student
  println(s"Podatek studenta wynosi: ${osoba.podatek}%")

  val osoba1 = new Osoba1("B", "B") with Pracownik
  println(s"Podatek pracownika wynosi ${osoba1.podatek}%")

  val osoba2 = new Osoba1("C", "C") with Nauczyciel
  println(s"Podatek nauczyciela wynosi ${osoba2.podatek}%")

  val osoba3 = new Osoba1("D", "D") with Student with Pracownik
  println(s"Podatek pracującego studenta wynosi ${osoba3.podatek}%")

  val osoba4 = new Osoba1("E", "E") with Pracownik with Student
  println(s"Podatek studiującego pracownika wynosi ${osoba4.podatek}%")
}





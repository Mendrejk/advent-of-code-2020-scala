import scala.annotation.tailrec
import scala.io.Source

object Day_1 {
  def main(args: Array[String]): Unit = {
    val directory: String = "resources/DataDay_1.txt"
    val data: List[Int] = readNumbers(directory)
    println(findTwoAddingUpTo(data, 2020))
    println(findThreeAddingUpTo(data, 2020))
  }

  def findTwoAddingUpTo(numbers: List[Int], targetSum: Int): List[Int] = {
    mapWithTail(numbers, {
      (head: Int, tail: List[Int]) =>
        for (x <- tail if x + head == targetSum) yield x * head })
      .flatten
  }

  def findThreeAddingUpTo(numbers: List[Int], targetSum: Int): List[Int] = {
    mapWithTail(numbers, {
      (first: Int, tail: List[Int]) =>
        mapWithTail(tail, {
          (second: Int, xs: List[Int]) =>
            for (third <- xs if first + second + third == targetSum)
              yield first * second * third })
          .flatten })
      .flatten
  }

  def readNumbers(directory: String): List[Int] = {
    val bufferedReader = Source.fromFile(directory)
    val numbers: List[Int] = bufferedReader.getLines()
      .map((number: String) => number.toInt).toList
    bufferedReader.close()
    numbers
  }

  @tailrec
  def foreachWithTail[A] (list: List[A], function: (A, List[A]) => Unit): Unit = {
    list match {
      case x :: xs =>
        function(x, xs)
        foreachWithTail(xs, function)
      case Nil => ()
    }
  }

  def mapWithTail[A, B] (list: List[A], transform: (A, List[A]) => B
                        ): List[B] = {
    @tailrec
    def mapRec(list: List[A], result: List[B]): List[B] = {
      list match {
        case x :: xs => mapRec(xs, transform(x, xs) :: result)
        case Nil => result
      }
    }
    mapRec(list, Nil).reverse
  }

  def collectWithTail[A, B]
    (list: List[A], predicate: A => Boolean,
    transform: (A, List[A]) => B): List[B] = {
      @tailrec
      def collectRec(list: List[A], result: List[B]): List[B] = {
        list match {
          case x :: xs =>
            if (predicate(x)) collectRec(xs, transform(x, xs) :: result)
            else collectRec(xs, result)
          case Nil => result
        }
      }
    collectRec(list, Nil).reverse
  }

}

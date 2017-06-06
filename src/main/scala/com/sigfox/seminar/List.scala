package com.sigfox.seminar

import com.sigfox.seminar.ListSample.{intList, stringList}

import scala.annotation.tailrec

/**
  *
  */
sealed trait List[+A] {

}

object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object BasicListOperations{


  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(list: List[Int]): Int = list match {
    case Nil => 1
    case Cons(head, tail) => head * product(tail)
  }

  def concat(list: List[String]): String = list match {
    case Nil => ""
    case Cons(head, tail) => head + concat(tail)
  }

  // Notice the similarities ?

}

object ListSample extends App{

  val intList: List[Int] = Cons(3, Cons(2, Cons(5, Nil)))
  val stringList: List[String] = Cons("a", Cons("b", Cons("c", Cons("d", Nil))))

  val sum = BasicListOperations.sum(intList)
  println(s"Int sum = $sum")

  val product = BasicListOperations.product(intList)
  println(s"Int product = $product")

  val stringConcat = BasicListOperations.concat(stringList)
  println(s"String concat = $stringConcat")

}

object FoldingOperations {

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def sum(list: List[Int]): Int = foldRight(list, 0){ (in, acc) =>
    acc + in
  }

  def product(list: List[Int]): Int = foldRight(list, 1){ (in, acc) => acc * in}

  def concat(list: List[String]): String = foldRight(list, ""){(in, acc) =>  in + acc} // Order is important :)

}

object FoldingInAction extends App {

  val intList: List[Int] = Cons(3, Cons(2, Cons(5, Nil)))
  val stringList: List[String] = Cons("a", Cons("b", Cons("c", Cons("d", Nil))))

  val sum = FoldingOperations.sum(intList)
  println(s"Int sum = $sum")

  val product = FoldingOperations.product(intList)
  println(s"Int product = $product")

  val stringConcat = FoldingOperations.concat(stringList)
  println(s"String concat = $stringConcat")

}

object MoreFoldingOperations {

  def reverse[A](list: List[A]): List[A] = MoreFoldingOperations.foldLeft[A,List[A]](list, Nil){ (acc, in) => Cons(in, acc)}

  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }


  // We can implement foldRight with foldLeft
  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = {
    // Fold left
    val inReverse = reverse(list)
    foldLeft(inReverse, z)((b, a) => f(a, b))
  }


  def sum(list: List[Int]) = foldLeft(list, 0)((acc, in) => acc + in) // order does not matter, + is commutative ie a + b === b + a

  def product(list: List[Int]) = foldLeft(list, 1)((acc, in) => acc * in) // order does not matter, * is commutative ie a * b === b * a

  def concat(list: List[String]) = foldLeft(list, "")((acc, in) => acc + in) // + is not commutative for String "ab" + "cd" =!= "cd" + "ab"


}

object ModifyingOperations {

  // Using folds
  def foldRightAdd3(list: List[Int]): List[Int] = MoreFoldingOperations.foldRight[Int, List[Int]](list, Nil)((in, acc) => Cons(in + 3, acc))



}

object MappingInAction extends App {

  val intList: List[Int] = Cons(3, Cons(2, Cons(5, Nil)))
  println(intList)
  println(MoreFoldingOperations.reverse(intList))
  println(ModifyingOperations.foldRightAdd3(intList))

}


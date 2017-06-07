package com.sigfox.seminar

/**
  *
  */
object Functions {

  // Basic operations with functions

  // Composing two functions, remember g∘f from high school ?
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = (a:A) => f(g(a))

  def partial[A,B,C](f: (A,B) =>C, a: A): B=>C = (b: B) => f(a, b)

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: (A => B => C)): (A, B) => C = (a: A, b: B) => f(a)(b)

}

object PlayingWithFunctions extends App {

  def plus(a: Int, b: Int): Int = a + b

  // Partially applying plus
  def double(a: Int): Int = Functions.partial(plus, 2)(a)

  def doubleThenAdd(toAdd: Int): Int => Int =
    a => Functions.partial(plus, toAdd).compose(double)(a)

  println(plus(3, 4))

  println(double(4))

  println(doubleThenAdd(2)(3))

}

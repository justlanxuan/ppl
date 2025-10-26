package lab1

import scala.annotation.tailrec

object Recursion:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if c == 0 || c == r then 1
    else pascal(c-1,r-1)+pascal(c,r-1)
    
  

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    @tailrec
    def loop(chars: List[Char],paras: Int): Boolean = chars match
      case _ if paras < 0 => false
      case '('::tail => loop(tail,paras + 1)
      case ')'::tail => loop(tail, paras - 1)
      case Nil if paras == 0 => true
      case Nil => false
      case _ => loop(chars.tail, paras)
    loop(chars,0)


  /**
   * Exercise 3
   * Write a recursive function that counts how many different ways you can make change for an amount, given a list of coin denominations. For example, there are 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.

Do this exercise by implementing the countChange function in Recursion.scala. This function takes an amount to change, and a list of unique denominations for the coins. Its signature is as follows:

def countChange(money: Int, coins: List[Int]): Int
Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.

Hint: Think of the degenerate cases. How many ways can you give change for 0 HKD? How many ways can you give change for > 0 HKD, if you have no coins?
Hint: you can reuse the coin
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if money < 0 then 0
    else if money == 0 then 1
    else coins match
      case head::tail=>countChange(money-head,coins) + countChange(money,tail)
      case Nil => 0
    



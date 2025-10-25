error id: file://<WORKSPACE>/src/main/scala/lab1/Recursion.scala:
file://<WORKSPACE>/src/main/scala/lab1/Recursion.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 504
uri: file://<WORKSPACE>/src/main/scala/lab1/Recursion.scala
text:
```scala
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
    if c == 0 || r == c then 1
    else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    @tailrec
    def loop(chars: List[Char], @@acc)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???

```


#### Short summary: 

empty definition using pc, found symbol in pc: 
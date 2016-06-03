package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("(lkjljjl)kjhkhk(kjhkjhjkh)".toList))
    println(countChange(-4, List(1,2)))
  }
//
  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || c > r) 0
    else if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def bal(chars: List[Char], counter: Int = 0): Boolean = {
      if (chars.isEmpty && counter == 0) true
      else if (chars.isEmpty && counter != 0 || chars.head == ')' && counter == 0) false
      else if (chars.head == ')') bal(chars.tail, counter - 1)
      else if (chars.head == '(') bal(chars.tail, counter + 1)
      else bal(chars.tail, counter)
    }
    bal(chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)


}

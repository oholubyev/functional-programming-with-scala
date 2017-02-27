package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (r == 0 || c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
  // refactor -- maybe score should not change external state
    def balance(chars: List[Char]): Boolean = {
      var count = 0
      var result = true

      def score(chars: List[Char]): Unit = {
        if (!chars.isEmpty) {
          chars.head match {
            case '(' => count += 1
            case ')' => count -= 1
            case _ => ;
          }
          if (count < 0) result = false
          else score(chars.tail)
        }
      }
      score(chars)
      result && count == 0
    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var count = 0
      def fit(money: Int, coins: List[Int]): Unit = {
        if (!coins.isEmpty && money > 0) {

          fit(money, coins.tail)

          val x = money - coins.head

          if (x == 0) count += 1
          else if (x > 0) fit(x, coins)
        }
      }
//      println(money, coins.sorted.reverse)
      fit(money, coins.sorted.reverse)
      count
    }
  }

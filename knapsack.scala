case class Item(weight: Int, value: Int)

  def knapsack(items: Seq[Item], capacity: Int): Int = {
    val n = items.length
    val dp = Array.ofDim[Int](n + 1, capacity + 1)

    for {
      i <- 1 to n
      w <- 0 to capacity
    } {
      dp(i)(w) =
        if (items(i - 1).weight <= w) {
          math.max(dp(i - 1)(w), items(i - 1).value + dp(i - 1)(w - items(i - 1).weight))
        } else {
          println(s"else i = $i w = $w - value= ${dp(i - 1)(w)} ${dp.map(_.mkString("-")).mkString(",")}")
          dp(i - 1)(w)
        }
    }

    dp(n)(capacity)
  }

val items = Seq(Item(2, 3), Item(3, 4), Item(4, 5), Item(5, 6))
val capacity = 5

  // Solve the knapsack problem
  val maxValue = knapsack(items, capacity)

  // Print the result
  println(s"Maximum value in the knapsack: $maxValue")

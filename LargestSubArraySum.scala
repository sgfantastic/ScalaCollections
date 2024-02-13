object LargestSubArraySumApp extends App{
/*
PS;- Given an array arr[] of size N. The task is to find the sum of the contiguous subarray within a arr[] with the largest sum.
i/p = {-2, -3, 4, -1, -2, 1, 5, -3}
o/p =  {4, -1, -2, 1, 5}

Kadane’s algorithm
 */

  val llst = Seq(-2, -3, 4, -1, -2, 1, 5, -3)

  def maxSubArraySum(nums: Seq[Int]): Int = {
    var maxEndingHere = nums.head
    var maxSoFar = nums.head

    for (i <- 1 until nums.length) {
      maxEndingHere = math.max(nums(i), maxEndingHere + nums(i))
      maxSoFar = math.max(maxSoFar, maxEndingHere)
    }

    maxSoFar
  }

  def maxSubArraySum1(nums: Seq[Int], tmpMax: Int ,acc: Int): Int = {
    nums match {
      case Nil => acc
      case x :: rest =>
        val tmpMaxEnding = math.max(x, tmpMax+ x)
        val tmpAcc = math.max(acc, tmpMaxEnding)
        maxSubArraySum1(rest, tmpMaxEnding,tmpAcc)
    }
  }

  val maxValue = llst.foldLeft(llst.head, llst.head)((acc, v) => {
    val (tmpMaxEnd, maxSofar) = acc
    val newTmpMaxEnd = math.max(v, tmpMaxEnd +v)
    val newMaxSoFar = math.max(maxSofar, newTmpMaxEnd)
    (newTmpMaxEnd,newMaxSoFar)
  })

  println(maxValue)

  println(maxSubArraySum1(llst, llst.head, llst.head))


  def maxSubArray(nums: Seq[Int], tmpMax: Seq[Int] = Nil, acc: Seq[Int]= Nil): Seq[Int] = {
    nums match {
      case Nil => acc
      case x :: rest =>
        val tmp =  tmpMax :+ x
        val newTmpMax = if(x > tmp.sum) Seq(x) else tmp
        val newAcc = if(acc.sum > newTmpMax.sum) acc else newTmpMax
        maxSubArray(rest, newTmpMax, newAcc)
    }
  }

  val maxArray = llst.foldLeft(Seq[Int](), Seq[Int]())((acc, v) => {
    val (tmpArray, tmpMax) = acc
    val tmp = tmpArray :+ v
    val newTmpArray =  if(v> tmp.sum) Seq(v) else tmp
    val newArr = if(tmpMax.sum > newTmpArray.sum) tmpMax else newTmpArray
    (newTmpArray,newArr)
  })

  println(maxSubArray(llst))
  println(maxArray)

}

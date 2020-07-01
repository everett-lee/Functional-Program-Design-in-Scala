package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for
  {
    value <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("finds correct minimum") = forAll { (a: Int, b: Int) =>
    val minimum = Math.min(a, b)
    val h = insert(a, insert(b, empty))
    findMin(h) == minimum
  }

  property("is empty following deletion of only item") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("is sorted in order of deletions") = forAll { (h: H) =>

    val firstMin = findMin(h)
    sortedAfterDeletionHelper(h, firstMin)
  }

  def sortedAfterDeletionHelper(h: H, min: Int): Boolean = {
    if (isEmpty(h)) true
    else {
      val nextH = deleteMin(h)
      val thisMin = findMin(h)

      min <= thisMin && sortedAfterDeletionHelper(nextH, thisMin)
    }
  }

  property("min of two melded heaps if min of both") = forAll { (h1: H, h2: H) =>
    val firstMin = findMin(h1)
    val secondMin = findMin(h2)
    val globalMin = Math.min(firstMin, secondMin)

    globalMin == findMin(meld(h1, h2))
  }
}

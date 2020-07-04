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
    deleteMin(h) == empty
  }

  property("is sorted in order of deletions") = forAll { (h: H) =>
    val seq = getSequence(h, Seq())

    seq == seq.sortWith((a,b) => a < b);
  }

  property("melded is sorted in order of deletions") = forAll { (h1: H, h2: H) =>
    val meldedH = meld(h1, h2)
    val seq = getSequence(meldedH, Seq())

    seq == seq.sortWith((a,b) => a < b);
  }

  property("moving element from one heap to another and melding results in same sequence melding" +
    "the original two heaps") = forAll { (h1: H, h2: H) =>
    val addedElem = findMin(h1)
    val h1Minus = deleteMin(h1)
    val h2Plus = insert(addedElem, h2)

    val origMeld = meld(h1, h2)
    val newMeld = meld(h1Minus, h2Plus)

    val origSeq = getSequence(origMeld, Seq())
    val newSeq = getSequence(newMeld, Seq())

    origSeq == newSeq;
  }

  def getSequence(h: H, seq: Seq[Int]): Seq[Int] = {
    if (isEmpty(h)) seq
    else {
      val nextH = deleteMin(h)
      val thisMin = findMin(h)

      getSequence(nextH, seq :+ thisMin)
    }
  }

  property("min of two melded heaps is min of either") = forAll { (h1: H, h2: H) =>
    val firstMin = findMin(h1)
    val secondMin = findMin(h2)

    val meldMin = findMin(meld(h1, h2));
    firstMin == meldMin || secondMin == meldMin
  }

  property("min of empty and heap is min of heap") = forAll { (h: H) =>
    val min = findMin(h)

    min == findMin(meld(empty, h))
  }

  property("heap melded with empty is itself") = forAll { (h: H) =>
    h == meld(empty, h)
  }

  property("heap melded with itself has same min") = forAll { (h: H) =>
    val min = findMin(h)

    min == findMin(meld(h, h))
  }
}

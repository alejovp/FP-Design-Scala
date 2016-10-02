package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- frequency((2, const(empty)), (8, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
   * If you insert an element into an empty heap, 
   * then find the minimum of the resulting heap, you get the element back 
   */
  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  /**
   * If you insert any two elements into an empty heap, finding the minimum
   * of the resulting heap should get the smallest of the two elements back
   */
  property("min2") = forAll { (a: A, b: A) =>
    val h = insert(a, empty)
    val l = insert(b, h)
    if (a < b) findMin(l) == a
    else findMin(l) == b
  }

  /**
   * If you insert an element into an empty heap, then delete the minimum, 
   * the resulting heap should be empty.
   */
  property("delete1") = forAll { a: A =>
    val e = empty
    val h = insert(a, e)
    deleteMin(h) == e
  }

  /**
   * Given any heap, you should get a sorted sequence of elements when 
   * continually finding and deleting minima. 
   * (Hint: recursion and helper functions are your friends.)
   */
  property("sorted1") = forAll { h: H =>
    def sortedSeq(h: H, acc: List[A]): List[A] = {
      if (isEmpty(h)) acc
      else sortedSeq(deleteMin(h), acc ::: findMin(h) :: Nil)
    }
    val hList = sortedSeq(h, Nil)

    hList == hList.sorted
  }

  /**
   * Finding a minimum of the melding of any two heaps 
   * should return a minimum of one or the other.
   */
  property("min3 meld1") = forAll { (a: H, b: H) =>
    val m = meld(a, b)
    findMin(m) == Math.min(findMin(a), findMin(b))
  }

  /**
   * Melding any Heap with the empty Heap should return
   * the initial Heap.
   */
  property("meld2") = forAll { (a: H) =>
    val m = meld(a, empty)
    m == a
  }

  /**
   * Melding two Heaps results in a new Heap that contains all the 
   * elements of the first Heap and all the elements of the second Heap.
   */
  property("meld3") = forAll { (a: H, b: H) =>
    val m = meld(a, b)
    def counter(h: H, c: Int): Int = {
      if (isEmpty(h)) c
      else counter(deleteMin(h), c + 1)
    }
    counter(m, 0) == counter(a, 0) + counter(b, 0)
  }

  /**
   * Given any two Heaps, the elements contained in the melding result of 
   * these initial Heaps should be the same as the elements contained in the melding
   * result of these Heaps but now with the minimun of one of them inserted in
   * the other one.
   */
  property("meld4") = forAll { (x: H, y: H) =>
    def sortedSeq(h: H, acc: List[A]): List[A] = {
      if (isEmpty(h)) acc
      else sortedSeq(deleteMin(h), acc ::: findMin(h) :: Nil)
    }
    val a = findMin(x)
    val b = deleteMin(x)
    val c = insert(a, y)
    val m1 = meld(x, y)
    val m2 = meld(b, c)
    
    sortedSeq(m1, Nil) == sortedSeq(m2, Nil)
  }

}

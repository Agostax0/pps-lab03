package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h,t) if n > 0 => skip(t)(n-1)
      case Cons(h,t) if n == 0 => Cons(h,t)
      case _ => Nil()
    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons (h2, t2)) => Cons( Tuple2(h1, h2) , zip( t1, t2) )
      case _ => Nil()
    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h,t) => Cons(h, concat(t , s2))
      case _ => s2
    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      def _reverse(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) => _reverse(t, Cons(h, acc))
        case _ => acc

      _reverse(s, Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      def _min(s: Sequence[Int], min: Int): Int = s match
        case Cons(h,t) if h < min => _min(t, h)
        case _ => min

      s match
        case Cons(h, t) => Just(_min(t, h))
        case _ => Empty()

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def traversalIndexed[A](s:Sequence[A], index: Int): Sequence[(A, Int)] = s match
      case Cons(h,t) => Cons((h,index), traversalIndexed(t,index+1))
      case _ => Nil()

    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      map(
        filter(traversalIndexed(s,0))
          ( s => s._2 % 2 == 0)
      )
        (s => s._1)



    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) => (h equals elem) | contains(t)(elem)
      case Nil() => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      def _distinct[A](s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) if !contains(t)(h) => _distinct(t, Cons(h, acc))
        case Cons(_, t) => _distinct(t, acc)
        case Nil() => acc

      _distinct(reverse(s), Nil())
    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =

      def _allInstancesIndexed[A](s: Sequence[(A, Int)])(elem: A): Sequence[(A, Int)] = filter(s)(seq => seq._1 == elem)

      def _filterOnlyContiguous[A](s: Sequence[(A, Int)])(acc: Sequence[A]): Sequence[A] = s match
        case Cons(h, Cons(h2, t)) if h._2 + 1 == h2._2 => _filterOnlyContiguous(t)(Cons(h._1,Cons(h2._1,acc)))
        case _ => acc

      println(s)

      val res = map(distinct(s)) ( h =>
        /**
         * Cons(
         *  Cons((10,0),Cons((10,1),Nil())),
         *  Cons(Cons((20,2),Cons((20,4),Nil())),
         *  Cons(Cons((30,3),Nil()),
         *  Nil())))
         */
        _filterOnlyContiguous( _allInstancesIndexed(traversalIndexed(s,0)) (h) )(Nil())

        /**
         * Cons(
         *  Cons(
         *    10,
         *    Cons(
         *      10,
         *      Nil())),
         *  Cons(
         *    Nil(),
         *    Cons(
         *      Nil(),
         *      Nil()
         *    )
         *  )
         * )
         */

      )

      println(res)




      res
    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = (filter(s)(pred), filter(s)(e => !pred(e)))

  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52

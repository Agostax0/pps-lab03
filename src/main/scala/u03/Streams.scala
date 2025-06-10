package u03
import u03.Sequences.Sequence.Cons
object Streams extends App:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()
    
    def interleave[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = (stream1, stream2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))
      case (Cons(h1, t1), _) => cons(h1(), interleave(t1(), empty()))
      case (_, Cons(h2, t2)) => cons(h2(), interleave(empty(), t2()))
      case _ => empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n-1)(k))
      case _ => Empty()

    def fibonacci(): Stream[Int] =
      def _fibonacci(n0: Int)(n1: Int): Stream[Int] =
        cons(n0, _fibonacci(n1)(n0+n1))

      _fibonacci(0)(1)


    def cycle[A](lst : Sequence[A]): Stream[A] =
      def _cycle[A](originalList: Sequence[A])(it: Sequence[A]): Stream[A] = it match
        case Sequence.Cons(h, t) => cons(h, _cycle(originalList)(it) )
        case _ => _cycle(originalList)(originalList)
      _cycle(lst)(lst)


  end Stream
end Streams

@main def tryStreams =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

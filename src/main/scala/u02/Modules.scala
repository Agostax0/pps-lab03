package u02

import u03.Sequences.*
import u03.Sequences.Sequence.*


object Modules extends App:

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def course(p: Person): String = p match
      case Teacher(_, c) => c
      case _ => ""

  println(Person.name(Person.Student("mario", 2015)))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  def courses(persons: Sequence[Person]): Sequence[String] = map(filter(persons)(!isStudent(_)))(course)

  def foldLeft[A,B](s: Sequence[A])(acc: B)(op: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(op(acc, h))(op)
    case _ => acc

  def countCourses(persons: Sequence[Person]): Int =
    foldLeft(map(courses(persons))(_ => 1))(0)(_ + _)


  println(isStudent(Student("mario", 2015)))
end Modules

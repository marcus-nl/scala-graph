package scalax.collection

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.mutable

trait TraversingIterable[+A] extends Iterable[A] {
//or perhaps: trait TraversingOps[A] { this: Iterable[A] =>

  private type CC[X] = Iterable[X]
  private type Builder[X] = mutable.Builder[X @uV, Iterable[X] @uV]

  // to be overridden in subclasses
  override def foreach[U](f: A => U): Unit

  def iterator = {
    withBuilder[A](b => for (x <- this) b += x).iterator
  }

  // Map Operations

  override def map[B](f: A => B): Iterable[B] =
    withBuilder[B](b => for (x <- this) b += f(x))

  override def flatMap[B](f: A => IterableOnce[B]): Iterable[B] =
    withBuilder[B](b => for (x <- this) b ++= f(x))

  override def collect[B](pf: PartialFunction[A, B]): Iterable[B] =
    withBuilder[B](b => foreach(pf.runWith(b += _)))

  // Size info

  override def isEmpty: Boolean  = headOption.isEmpty
  override def nonEmpty: Boolean = !isEmpty

  // Element Retrieval

  override def head: A = headOption.get

  override def headOption: Option[A] = {
    for (x <- this) return Some(x)
    None
  }

  override def find(p: A => Boolean): Option[A] = {
    for (x <- this)
      if (p(x)) return Some(x)
    None
  }

  override def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    val sentinel: A => Any = _ => this
    for (x <- this) {
      val r = pf.applyOrElse(x, sentinel)
      if (r.asInstanceOf[AnyRef] ne sentinel) return Some(r.asInstanceOf[B])
    }
    None
  }

  // Subcollections

  override def take(n: Int): Iterable[A] = slice(0, n)

  override def takeWhile(p: A => Boolean): Iterable[A] = {
    def block(b: Builder[A]): Unit = {
      for (x <- this) {
        if (!p(x)) return
        b += x
      }
    }
    withBuilder(block)
  }

  override def slice(from: Int, until: Int) = {
    math.max(from, 0) pipe { from =>
      def block(b: Builder[A]): Unit =
        if (until > from) {
          var i = 0
          for (x <- this) {
            if (i >= from) b += x
            i += 1
            if (i >= until) return
          }
        }
      withBuilder(block)
    }
  }

  // Folds

  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this) result = op(result, x)
    result
  }

  // Element Conditions

  override final def forall(p: A => Boolean): Boolean = {
    for (x <- this)
      if (!p(x)) return false
    true
  }

  override final def exists(p: A => Boolean): Boolean = {
    for (x <- this)
      if (p(x)) return true
    false
  }

  // Builder utilities

  // TODO sizeHint
  private def withBuilder[B](block: Builder[B] => Unit): Iterable[B] = {
    val b = iterableFactory.newBuilder[B]
    block(b)
    b.result()
  }
}

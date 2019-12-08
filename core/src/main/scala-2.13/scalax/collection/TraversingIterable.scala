package scalax.collection

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.mutable

trait TraversingIterable[+A] extends Iterable[A] {
//or perhaps: trait TraversingOps[A] { this: Iterable[A] =>

  type Builder = mutable.Builder[A @uV, Iterable[A] @uV]

  // to be overridden in subclasses
  override def foreach[U](f: A => U): Unit

  def iterator = {
    // hmmm...
    withBuilder { b =>
      for (x <- this) b += x
    }.iterator
  }

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
    def block(b: Builder): Unit = {
      for (x <- this) {
        if (!p(x)) return
        b += x
      }
    }
    withBuilder(block)
  }

  override def slice(from: Int, until: Int) = {
    math.max(from, 0) pipe { from =>
      def block(b: Builder): Unit =
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
  private def withBuilder(block: Builder => Unit): Iterable[A] = {
    val b = newSpecificBuilder
    block(b)
    b.result()
  }
}

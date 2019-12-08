package scalax.collection

object Compat {
  type AbstractTraversable[+A] = scalax.collection.AbstractTraversable[A]

  implicit final class IterableEnrichments[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to(MSet)
  }

  trait Growable[-A]
  trait AddSubtract[A, +This]
  trait InclExcl[A, +This]

  trait ExtBitSet
}

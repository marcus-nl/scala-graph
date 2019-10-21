package scalax.collection

object Compat {
  implicit final class ToExts[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to[MSet]
  }
}

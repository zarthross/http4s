package org.http4s.util

object tap {
  implicit class TapSyntax[A <: AnyRef](val self: A) extends AnyVal {
    /** If you're using this, you're in a dark place. */
    final def tap(f: A => Unit): self.type = {
      f(self)
      self
    }
  }
}

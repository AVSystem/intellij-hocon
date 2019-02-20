package org.jetbrains.plugins.hocon.psi

/**
  * "Something that has keys in it and stuff associated with those keys".
  */
trait HScope { outer =>
  def directKeyedFields(reverse: Boolean = false): Iterator[HKeyedField]

  def directSubScope(key: String): HScope = new HScope {
    def directKeyedFields(reverse: Boolean): Iterator[HKeyedField] =
      outer.directKeyedFields(reverse)
        .filter(_.validKey.exists(_.stringValue == key))
        .flatMap(_.subScopes(reverse))
        .flatMap(_.directKeyedFields(reverse))
  }
}

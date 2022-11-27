package pprint

object StringPrefix {
  def apply(i: Iterable[_]) =
    scala.collection.internal.pprint.CollectionName.get(i)
}

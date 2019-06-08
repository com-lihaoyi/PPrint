package pprint

object StringPrefix{
  def apply(i: Iterable[_]) = i.asInstanceOf[{ def collectionClassName: String }].collectionClassName
}
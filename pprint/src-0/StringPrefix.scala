package pprint

import reflect.Selectable.reflectiveSelectable

object StringPrefix{
  def apply(i: Iterable[_]) = i.asInstanceOf[{ def collectionClassName: String }].collectionClassName
}

package pprint

object ProductSupport {

  def treeifyProductElements(x: Product,
                             walker: Walker,
                             escapeUnicode: Boolean,
                             showFieldNames: Boolean): Iterator[Tree] = {
    if (!showFieldNames) x.productIterator.map(x => walker.treeify(x, escapeUnicode, showFieldNames))
    else x.productElementNames
      .zipWithIndex
      .map {
        case (name, i) =>
          val elem = x.productElement(i)
          Tree.KeyValue(name, walker.treeify(elem, escapeUnicode, showFieldNames))
      }
  }

}

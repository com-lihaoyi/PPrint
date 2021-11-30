package pprint

object ProductSupport {

  def treeifyProductElements(x: Product,
                             walker: Walker,
                             escapeUnicode: Boolean,
                             showFieldNames: Boolean): Iterator[Tree] =
    x.productIterator.map(x => walker.treeify(x, escapeUnicode, showFieldNames))

}

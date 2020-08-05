package pprint

object ProductSupport {

  def treeifyProductElements(x: Product, walker: Walker): Iterator[Tree] =
    x.productIterator.map(x => walker.treeify(x))

}

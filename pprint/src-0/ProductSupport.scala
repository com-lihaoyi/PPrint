package pprint

object ProductSupport {

  def treeifyProductElements(x: Product, walker: Walker): Iterator[Tree] = {
    if (!walker.showFieldNames) x.productIterator.map(x => walker.treeify(x))
    else x.productElementNames
      .zipWithIndex
      .map {
        case (name, i) =>
          val elem = x.productElement(i)
          Tree.KeyValue(name, walker.treeify(elem))
      }
  }

}

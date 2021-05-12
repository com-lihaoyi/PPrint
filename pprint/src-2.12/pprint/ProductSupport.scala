package pprint

object ProductSupport {

  def treeifyProductElements(x: Product, walker: Walker): Iterator[Tree] =
    x.productIterator.map(x => walker.treeify(x))

  // Implementation taken from https://stackoverflow.com/questions/15718506/scala-how-to-print-case-classes-like-pretty-printed-tree/57080463#57080463
  def caseClassToMap(cc: Product): Map[String, Any] = {
    val fieldValues = cc.productIterator.toSet
    val fields = cc.getClass.getDeclaredFields.toSeq
      .filterNot(f => f.isSynthetic || java.lang.reflect.Modifier.isStatic(f.getModifiers))
    fields.map { f =>
      f.setAccessible(true)
      f.getName -> f.get(cc)
    }.filter { case (k, v) => fieldValues.contains(v) }
      .toMap
  }
}

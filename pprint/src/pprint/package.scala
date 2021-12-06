/**
  * Contains a convenient default pre-configured PPrinter.
  *
  * Hard-coded and inflexible, but feel free to instantiate your own
  * PPrint if you want to customize it.
  */
package object pprint extends PPrinter{
  def tprint[T: TPrint](implicit config: TPrintColors) = {
    implicitly[TPrint[T]].render
  }
}

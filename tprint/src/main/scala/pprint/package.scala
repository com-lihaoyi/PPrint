package object pprint {
  def tprint[T: TPrint](implicit config: TPrintColors) = {
    implicitly[TPrint[T]].render(config)
  }
}


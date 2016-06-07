package hlist

/**
  * @author bogdanski
  * @since 07.06.16
  */

trait HMapper0[L <: HList] extends HFunctor0[L] {
  def fs: HList
}

object HMapper {
  type Aux0[L <: HList, R <: HList] = HMapper0[L] { type Res = R }
}

trait Func[X, Y] {
  val f: X => Y
}

package hlist

/**
  * @author bogdanski
  * @since 03.06.16
  */

trait HFunctor0[L <: HList] {
  type Res <: HList
}

trait HFunctor1[L <: HList, F[_]] {
  type Res <: HList
}

trait HFunctor2[L <: HList, F[_], G[_]] {
  type Res <: HList
}

object HFunctor {
  type Aux0[L <: HList, R <: HList] = HFunctor0[L] { type Res = R }
  type Aux1[L <: HList, R <: HList, F[_]] = HFunctor1[L, F] { type Res = R }
  type Aux2[L <: HList, R <: HList, F[_], G[_]] = HFunctor2[L, F, G] { type Res = R }
}

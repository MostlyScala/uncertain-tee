package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain

trait NestedOps {
  extension [T](nested: Uncertain[Uncertain[T]]) {

    /** Transforms the nested `Uncertain[Uncertaion[Option[T]]]` into Uncertain[T] */
    def flatten: Uncertain[T] = nested.flatMap(identity)
  }

}

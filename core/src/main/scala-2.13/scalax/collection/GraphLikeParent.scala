package scalax.collection

import language.higherKinds
import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.generic.GraphCompanion

import scala.collection.{SetOps, mutable}

trait GraphLikeBase[N,
                    E[+X] <: EdgeLikeIn[X],
                    +This[NN, EE[+XX] <: EdgeLikeIn[XX]] <: GraphLike[NN, EE, This] with AnySet[Param[NN, EE]] with Graph[NN, EE]]
  extends AnySet[Param[N, E]]
     with SetOps[Param[N, E], AnySet, This[N, E]] {
  thisGraph: This[N, E] with GraphLikeBase[N, E, This] with AnySet[Param[N, E]] with Graph[N, E] =>

  /** The companion object of `This`. */
  val graphCompanion: GraphCompanion[This]

  override def empty: This[N, E] = graphCompanion.empty[N, E]
  override protected def fromSpecific(coll: IterableOnce[Param[N, E]]): This[N, E] = graphCompanion.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[Param[N, E], This[N, E]] = graphCompanion.newBuilder
}

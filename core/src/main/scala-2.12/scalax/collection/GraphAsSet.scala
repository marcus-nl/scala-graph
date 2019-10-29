package scalax.collection

import scala.language.higherKinds

import scalax.collection.Compat.InclExcl
import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.generic.GraphCompanion

private[collection] trait GraphAsSet[N,
                                     E[+X] <: EdgeLikeIn[X],
                                     +This[NN, EE[+XX] <: EdgeLikeIn[XX]] <: GraphLike[NN, EE, This] with AnySet[
                                       Param[NN, EE]] with Graph[NN, EE]]
  extends scala.collection.SetLike[Param[N, E], This[N, E]]
  with InclExcl[Param[N, E], This[N, E]] {

  /** The companion object of `This`. */
  val graphCompanion: GraphCompanion[This]

  /** Needs to be defined here so it can be overridden in trait scalax.collection.Graph */
  def knownSize: Int = -1
}

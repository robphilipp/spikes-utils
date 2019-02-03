package com.digitalcipher.spiked.topology.coords

/**
  * Created by rob on 12/18/16.
  */
trait Measure[M, X1, X2, X3] {

  /**
    * The distance between two points under the specified measure
    * @param point1 The first point
    * @param point2 The second point
    * @return The distance in the units <M>
    */
  def distance( point1: (X1, X2, X3), point2: (X1, X2, X3) ): M

  def norm( point: (X1, X2, X3) ): M
}

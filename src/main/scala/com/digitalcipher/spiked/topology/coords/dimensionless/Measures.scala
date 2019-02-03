package com.digitalcipher.spiked.topology.coords.dimensionless

import com.digitalcipher.spiked.topology.coords.Measure

/**
  * Created by rob on 12/18/16.
  */
object Measures {

  trait Dimensionless[X1, X2, X3] extends Measure[Double, X1, X2, X3] {
    /**
      * The norm of the point
      * @param point The tuple representing the point
      * @return The norm (magnitude) of the point
      */
    def norm( point: (X1, X2, X3) ): Double
  }

  /**
    * The cartesian measure given by ((x,,1,,-x,,2,,)^2^ + (y,,1,,-y,,2,,)^2^ + (z,,1,,-z,,2,,)^2^ )^1/2^
    */
  object Cartesian extends Dimensionless[Double, Double, Double] {

    /**
      * The distance between two Cartesian points
      *
      * @param point1 The first point
      * @param point2 The second point
      * @return The distance in the units <M>
      */
    override def distance(point1: (Double, Double, Double), point2: (Double, Double, Double)): Double =
      norm( Points.Cartesian(point1).subtract(Points.Cartesian(point2)).tuple )

    /**
      * The norm (or magnitude of the vector from the origin to the point)
      * @param point The cartesian point
      * @return The distance from the origin to the point
      */
    override def norm(point: (Double, Double, Double)): Double =
      math.sqrt(point._1 * point._1 + point._2 * point._2 + point._3 * point._3)
  }

  /**
    * The distance between the two cylindrical coordinates
    */
  object Cylindrical extends Dimensionless[Double, Double, Double] {

    /**
      * The distance between two Cylindrical points
      *
      * @param point1 The first point
      * @param point2 The second point
      * @return The distance in the units <M>
      */
    override def distance(point1: (Double, Double, Double), point2: (Double, Double, Double)): Double =
      norm( Points.Cylindrical(point1).subtract(Points.Cylindrical(point2)).tuple )

    /**
      * The norm (or magnitude of the vector from the origin to the point)
      * @param point The cylindrical point
      * @return The distance from the origin to the point
      */
    override def norm(point: (Double, Double, Double)): Double =
      math.sqrt(point._1 * point._1 + point._3 * point._3)
  }

  /**
    * The distance between two spherical coordinates
    */
  object Spherical extends Dimensionless[Double, Double, Double] {

    /**
      * The distance between two points under the specified measure
      *
      * @param point1 The first point
      * @param point2 The second point
      * @return The distance in the units <M>
      */
    override def distance(point1: (Double, Double, Double), point2: (Double, Double, Double)): Double =
      norm( Points.Spherical(point1).subtract(Points.Spherical(point2)).tuple)

    /**
      *
      * @param point The spherical point
      * @return The distance from the origin to the point in [[Double]] units
      */
    override def norm(point: (Double, Double, Double)): Double = point._1.abs
  }
}

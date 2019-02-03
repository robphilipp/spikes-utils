package com.digitalcipher.spiked.topology.coords.spatial

import com.digitalcipher.spiked.topology.coords.Measure
import squants.{Angle, Length}

/**
  * Created by rob on 12/18/16.
  */
object Measures {

  trait Spatial[X1, X2, X3] extends Measure[Length, X1, X2, X3] {
    /**
      * The norm (or magnitude of the vector from the origin to the point)
      * @param point The point
      * @return The distance from the origin to the point in [[Length]] units
      */
    def norm( point: (X1, X2, X3) ): Length
  }

  /**
    * The cartesian measure given by ((x,,1,,-x,,2,,)^2^ + (y,,1,,-y,,2,,)^2^ + (z,,1,,-z,,2,,)^2^ )^1/2^
    */
  object Cartesian extends Spatial[Length, Length, Length] {

    /**
      * The distance between two Cartesian points
      *
      * @param point1 The first point
      * @param point2 The second point
      * @return The distance in the units <M>
      */
    override def distance(point1: (Length, Length, Length), point2: (Length, Length, Length)): Length =
      norm( Points.Cartesian(point1).subtract(Points.Cartesian(point2)).tuple )

    /**
      * The norm (or magnitude of the vector from the origin to the point)
      * @param point The cartesian point
      * @return The distance from the origin to the point in [[Length]] units
      */
    override def norm(point: (Length, Length, Length)): Length =
      (point._1 * point._1 + point._2 * point._2 + point._3 * point._3).squareRoot
  }

  /**
    * The distance between the two cylindrical coordinates
    */
  object Cylindrical extends Spatial[Length, Angle, Length] {

    /**
      * The distance between two Cylindrical points
      *
      * @param point1 The first point
      * @param point2 The second point
      * @return The distance in the units <M>
      */
    override def distance(point1: (Length, Angle, Length), point2: (Length, Angle, Length)): Length =
      norm( Points.Cylindrical(point1).subtract(Points.Cylindrical(point2)).tuple )

    /**
      * The norm (or magnitude of the vector from the origin to the point)
      * @param point The cylindrical point
      * @return The distance from the origin to the point in [[Length]] units
      */
    override def norm(point: (Length, Angle, Length)): Length =
      (point._1 * point._1 + point._3 * point._3).squareRoot
  }

  /**
    * The distance between two spherical coordinates
    */
  object Spherical extends Spatial[Length, Angle, Angle] {

    /**
      * The distance between two points under the specified measure
      *
      * @param point1 The first point
      * @param point2 The second point
      * @return The distance in the units <M>
      */
    override def distance(point1: (Length, Angle, Angle), point2: (Length, Angle, Angle)): Length =
      norm( Points.Spherical(point1).subtract(Points.Spherical(point2)).tuple)

    /**
      *
      * @param point The spherical point
      * @return The distance from the origin to the point in [[Length]] units
      */
    override def norm(point: (Length, Angle, Angle)): Length = point._1.abs
  }
}

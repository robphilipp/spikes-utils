package com.digitalcipher.spiked.topology.coords

/**
  * Created by rob on 12/18/16.
  */
abstract class Coordinate[M, X1, X2, X3] {

  /**
    * @return The coordinates measure
    */
  def measure: Measure[M, X1, X2, X3]

  /**
    * @return The point
    */
  def point: Point[X1,X2,X3]

  /**
    * Subtracts the specified coordinate from this coordinate (vector subtraction)
    *
    * @param coordinate The coordinate to subtract from this one
    * @return The difference coordinate (vector) (this - specified)
    */
  def -(coordinate: Coordinate[M, X1, X2, X3]): Coordinate[M, X1, X2, X3]

  /**
    * Adds the specified coordinate to this one (vector addition)
    *
    * @param coordinate The coordinate to add to this one
    * @return The sum of the two coordinates
    */
  def +(coordinate: Coordinate[M, X1, X2, X3]): Coordinate[M, X1, X2, X3]

  /**
    * Calculates the distance between the two specified coordinates
    *
    * @param coordinate The first coordinate
    * @return The difference between the two coordinates
    */
  def distanceTo(coordinate: Coordinate[M, X1, X2, X3]): M = measure.distance( point.tuple, coordinate.point.tuple )

  /**
    * @return The norm of the coordinate (vector from origin)
    */
  def norm: M = measure.norm( this.point.tuple )
}

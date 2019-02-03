package com.digitalcipher.spiked.topology.coords

/**
  * Created by rob on 12/18/16.
  */
trait Point[X1, X2, X3] {

  /**
    * @return The point represented as a tuple
    */
  def tuple: (X1, X2, X3)

  /**
    * @return The first value of the point
    */
  def _1: X1 = tuple._1

  /**
    * @return The second value of the point
    */
  def _2: X2 = tuple._2

  /**
    * @return The third value of the point
    */
  def _3: X3 = tuple._3

  /**
    * Adds a point to this point (vector addition) and returns the resulting point. Assumes that the points are of
    * the same type
    *
    * @param point The point to add to this one
    * @return The (vector) sum of the two points
    */
  def add(point: Point[X1, X2, X3]): Point[X1, X2, X3]

  /**
    * Subtracts (vector subtraction) the specified point from this one
    *
    * @param point The point to subtract from this one
    * @return The (vector) difference of the two points
    */
  def subtract(point: Point[X1, X2, X3]): Point[X1, X2, X3]
}

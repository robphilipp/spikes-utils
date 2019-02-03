package com.digitalcipher.spiked.topology.coords.dimensionless

import com.digitalcipher.spiked.topology.coords.Coordinate

/**
  * Created by rob on 12/11/16.
  */
object Coordinates {

  /**
    * Abstract class representing spatial coordinates (i.e. coordinates in space)
    */
  abstract class Dimensionless[X1, X2, X3]( val measure: Measures.Dimensionless[X1, X2, X3]) extends Coordinate[Double, X1, X2, X3]

  /**
    * Creates a cartesian coordinate (x, y, z) of length dimensions
    *
    * @param tuple The point, which along with measure, defines the coordinate. The point consists of 3 lengths.
    */
  implicit class Cartesian(tuple: (Double, Double, Double)) extends Dimensionless[Double, Double, Double](Measures.Cartesian) {

    /**
      * @return The point
      */
    override val point: Points.Cartesian = Points.Cartesian(tuple)

    /**
      * Subtracts the specified coordinate from this coordinate (vector subtraction)
      *
      * @param coordinate The coordinate to subtract from this one
      * @return The difference coordinate (vector) (this - specified)
      */
    override def -(coordinate: Coordinate[Double, Double, Double, Double]): Cartesian =
      new Cartesian(point.subtract(coordinate.point).tuple)

    /**
      * Adds the specified coordinate to this one (vector addition)
      *
      * @param coordinate The coordinate to add to this one
      * @return The sum of the two coordinates
      */
    override def +(coordinate: Coordinate[Double, Double, Double, Double]): Cartesian =
      new Cartesian(point.add(coordinate.point).tuple)

    /**
      * @return A string representation of the cartesian point
      */
    override def toString: String = s"${Points.Cartesian(tuple)}, norm=${measure.norm(tuple)}"
  }

  /**
    * Creates a cylindrical coordinate (r, φ, z) with length dimensions for r and z, and φ is an angle
    *
    * @param tuple The point, which along with measure, defines the coordinate
    */
  implicit class Cylindrical(tuple: (Double, Double, Double)) extends Dimensionless[Double, Double, Double](Measures.Cylindrical) {

    /**
      * @return The point
      */
    override val point: Points.Cylindrical = Points.Cylindrical(tuple)

    /**
      * Subtracts the specified coordinate from this coordinate (vector subtraction)
      *
      * @param coordinate The coordinate to subtract from this one
      * @return The difference coordinate (vector) (this - specified)
      */
    override def -(coordinate: Coordinate[Double, Double, Double, Double]): Cylindrical =
      Cylindrical(point.subtract(coordinate.point).tuple)

    /**
      * Adds the specified coordinate to this one (vector addition)
      *
      * @param coordinate The coordinate to add to this one
      * @return The sum of the two coordinates
      */
    override def +(coordinate: Coordinate[Double, Double, Double, Double]): Cylindrical =
      Cylindrical(point.add(coordinate.point).tuple)

    /**
      * @return A string representation of the cylindrical coordinate
      */
    override def toString: String = s"${Points.Cylindrical(tuple)}, norm=${measure.norm(tuple)}"
  }

  /**
    * Spherical coordinates The point (r, φ, θ) is based an standard physics usage where
    * <ul>
    *   <li>r is the radial length</li>
    *   <li>φ is the angle between the x-axis and the projection of r onto the x-y plane</li>
    *   <li>θ is the angle between the z-axis and the r</li>
    * </ul>
    * @param tuple The spherical point (r, φ, θ)
    */
  implicit class Spherical(tuple: (Double, Double, Double)) extends Dimensionless[Double, Double, Double](Measures.Spherical) {
    /**
      * @return The point
      */
    override val point: Points.Spherical = Points.Spherical(tuple)

    /**
      * Subtracts the specified coordinate from this coordinate (vector subtraction)
      *
      * @param coordinate The coordinate to subtract from this one
      * @return The difference coordinate (vector) (this - specified)
      */
    override def -(coordinate: Coordinate[Double, Double, Double, Double]): Spherical =
      Spherical(point.subtract(coordinate.point).tuple)

    /**
      * Adds the specified coordinate to this one (vector addition)
      *
      * @param coordinate The coordinate to add to this one
      * @return The sum of the two coordinates
      */
    override def +(coordinate: Coordinate[Double, Double, Double, Double]): Spherical =
      Spherical(point.add(coordinate.point).tuple)

    /**
      * @return A string representation of the cylindrical coordinate
      */
    override def toString: String = s"${Points.Spherical(tuple)}, norm=${measure.norm(tuple)}"
  }
}
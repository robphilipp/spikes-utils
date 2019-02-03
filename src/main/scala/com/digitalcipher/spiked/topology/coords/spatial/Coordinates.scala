package com.digitalcipher.spiked.topology.coords.spatial

import com.digitalcipher.spiked.topology.coords.Coordinate
import squants.Angle
import squants.space.{Length, Microns, Radians}

/**
  * Created by rob on 12/11/16.
  */
object Coordinates {

  /**
    * Abstract class representing spatial coordinates (i.e. coordinates in space)
    */
  abstract class Spatial[X1, X2, X3]( val measure: Measures.Spatial[X1, X2, X3]) extends Coordinate[Length, X1, X2, X3]

  /**
    * Creates a cartesian coordinate (x, y, z) of length dimensions
    *
    * @param tuple The point, which along with measure, defines the coordinate. The point consists of 3 lengths.
    */
  implicit class Cartesian(tuple: (Length, Length, Length)) extends Spatial[Length, Length, Length](Measures.Cartesian) {

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
    override def -(coordinate: Coordinate[Length, Length, Length, Length]): Cartesian =
      new Cartesian(point.subtract(coordinate.point).tuple)

    /**
      * Adds the specified coordinate to this one (vector addition)
      *
      * @param coordinate The coordinate to add to this one
      * @return The sum of the two coordinates
      */
    override def +(coordinate: Coordinate[Length, Length, Length, Length]): Cartesian =
      new Cartesian(point.add(coordinate.point).tuple)

    /**
      * @return A string representation of the cartesian point
      */
    override def toString: String = s"${Points.Cartesian(tuple)}, norm=${measure.norm(tuple).in(Microns)}"
  }

  /**
    * The companion to the cartesian coordinate that converts the numeric values to millimeters
    */
  object Cartesian {

    /**
      * Conversion of a tuple of three numeric values into a tuple of [[Microns]]
      *
      * @param point The point consisting of numeric values
      * @param x1    The x-coordinate value of the tuple
      * @param x2    The y-coordinate value of the tuple
      * @param x3    The z-coordinate value of the tuple
      * @tparam X1 The type of the x-coordinate value
      * @tparam X2 The type of the y-coordinate value
      * @tparam X3 The type of the z-coordinate value
      * @return A [[Cartesian]] coordinate based on the 3 numeric values
      */
    def apply[X1, X2, X3](point: (X1, X2, X3))(implicit x1: Numeric[X1], x2: Numeric[X2], x3: Numeric[X3]) =
      new Cartesian((Microns(point._1), Microns(point._2), Microns(point._3)))
  }

  /**
    * Creates a cylindrical coordinate (r, φ, z) with length dimensions for r and z, and φ is an angle
    *
    * @param tuple The point, which along with measure, defines the coordinate
    */
  implicit class Cylindrical(tuple: (Length, Angle, Length)) extends Spatial[Length, Angle, Length](Measures.Cylindrical) {

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
    override def -(coordinate: Coordinate[Length, Length, Angle, Length]): Cylindrical =
      Cylindrical(point.subtract(coordinate.point).tuple)

    /**
      * Adds the specified coordinate to this one (vector addition)
      *
      * @param coordinate The coordinate to add to this one
      * @return The sum of the two coordinates
      */
    override def +(coordinate: Coordinate[Length, Length, Angle, Length]): Cylindrical =
      Cylindrical(point.add(coordinate.point).tuple)

    /**
      * @return A string representation of the cylindrical coordinate
      */
    override def toString: String = s"${Points.Cylindrical(tuple)}, norm=${measure.norm(tuple).in(Microns)}"
  }

  /**
    * The companion to the cylindrical coordinate that converts the numeric values to with length dimensions (r, z) into
    * millimeters, and the numeric value representing the angle (φ) into radians
    */
  object Cylindrical {

    /**
      * Conversion of a tuple of three numeric values into a tuple ([[Microns]], [[squants.space.Radians]], [[Microns]])
      *
      * @param point The point consisting of numeric values
      * @param x1    The r-coordinate value of the tuple
      * @param x2    The φ-coordinate value of the tuple
      * @param x3    The z-coordinate value of the tuple
      * @tparam X1 The type of the r-coordinate value
      * @tparam X2 The type of the φ-coordinate value
      * @tparam X3 The type of the z-coordinate value
      * @return A [[Cylindrical]] coordinate based on the 3 numeric values
      */
    def apply[X1, X2, X3](point: (X1, X2, X3))(implicit x1: Numeric[X1], x2: Numeric[X2], x3: Numeric[X3]) =
      new Cylindrical((Microns(point._1), Radians(point._2), Microns(point._3)))
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
  implicit class Spherical(tuple: (Length, Angle, Angle)) extends Spatial[Length, Angle, Angle](Measures.Spherical) {
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
    override def -(coordinate: Coordinate[Length, Length, Angle, Angle]): Spherical =
      Spherical(point.subtract(coordinate.point).tuple)

    /**
      * Adds the specified coordinate to this one (vector addition)
      *
      * @param coordinate The coordinate to add to this one
      * @return The sum of the two coordinates
      */
    override def +(coordinate: Coordinate[Length, Length, Angle, Angle]): Spherical =
      Spherical(point.add(coordinate.point).tuple)

    /**
      * @return A string representation of the cylindrical coordinate
      */
    override def toString: String = s"${Points.Spherical(tuple)}, norm=${measure.norm(tuple).in(Microns)}"
  }

  /**
    * Companion object to the [[Spherical]] class
    */
  object Spherical {
    /**
      * Conversion of a tuple of three numeric values into a tuple ([[Microns]], [[Radians]], [[Radians]])
      *
      * @param point The point consisting of numeric values
      * @param x1    The r-coordinate value of the tuple
      * @param x2    The φ-coordinate value of the tuple
      * @param x3    The θ-coordinate value of the tuple
      * @tparam X1 The type of the r-coordinate value
      * @tparam X2 The type of the φ-coordinate value
      * @tparam X3 The type of the θ-coordinate value
      * @return A [[Spherical]] coordinate based on the 3 numeric values
      */
    def apply[X1, X2, X3](point: (X1, X2, X3))(implicit x1: Numeric[X1], x2: Numeric[X2], x3: Numeric[X3]) =
      new Spherical((Microns(point._1), Radians(point._2), Radians(point._3)))
  }
}
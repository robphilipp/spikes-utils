package com.digitalcipher.spiked.topology.coords

import com.digitalcipher.spiked.topology.coords.dimensionless.Coordinates.{Cartesian,Cylindrical,Spherical}

/**
  * Created by rob on 12/26/16.
  */
package object dimensionless {

  /*
    CONVERSIONS: cartesian <--> spherical
   */

  /**
    * Implicit conversion for [[Cylindrical]] coordinates into [[Cartesian]] coordinates
    *
    * @param cylindricalCoordinate The cylindrical coordinate
    * @return a [[Cartesian]] coordinate
    */
  implicit def toCartesian(cylindricalCoordinate: Cylindrical): Cartesian = {
    Cartesian((
      Points.Cylindrical.toX(cylindricalCoordinate.point._1, cylindricalCoordinate.point._2),
      Points.Cylindrical.toY(cylindricalCoordinate.point._1, cylindricalCoordinate.point._2),
      cylindricalCoordinate.point._3
    ))
  }

  /**
    * Implicit conversion for [[Cartesian]] coordinates into [[Cylindrical]] coordinates
    *
    * @param cartesianCoordinate The cartesian coordinate
    * @return a [[Cylindrical]] coordinate
    */
  implicit def toCylindrical(cartesianCoordinate: Cartesian): Cylindrical = {
    Cylindrical((
      Points.Cylindrical.toR(cartesianCoordinate.point._1, cartesianCoordinate.point._2),
      Points.Cylindrical.toPhi(cartesianCoordinate.point._1, cartesianCoordinate.point._2),
      cartesianCoordinate.point._3
    ))
  }

  /*
    CONVERSIONS: cartesian <--> spherical
   */

  /**
    * Implicit conversion for [[Spherical]] coordinates into [[Cartesian]] coordinates
    *
    * @param sphericalCoordinate The spherical coordinate
    * @return a [[Cartesian]] coordinate
    */
  implicit def toCartesian(sphericalCoordinate: Spherical): Cartesian = {
    Cartesian((
      Points.Spherical.toX(sphericalCoordinate.point._1, sphericalCoordinate.point._2, sphericalCoordinate.point._3),
      Points.Spherical.toY(sphericalCoordinate.point._1, sphericalCoordinate.point._2, sphericalCoordinate.point._3),
      Points.Spherical.toZ(sphericalCoordinate.point._1, sphericalCoordinate.point._3)
    ))
  }

  /**
    * Implicit conversion for [[Cartesian]] coordinates into [[Spherical]] coordinates
    *
    * @param cartesianCoordinate The cartesian coordinate
    * @return a [[Spherical]] coordinate
    */
  implicit def toSpherical(cartesianCoordinate: Cartesian): Spherical = {
    Spherical((
      Points.Spherical.toR(cartesianCoordinate.point._1, cartesianCoordinate.point._2, cartesianCoordinate.point._3),
      Points.Spherical.toPhi(cartesianCoordinate.point._1, cartesianCoordinate.point._2),
      Points.Spherical.toTheta(cartesianCoordinate.point._1, cartesianCoordinate.point._2, cartesianCoordinate.point._3)
    ))
  }

  /*
  CONVERSIONS: cylindrical <--> spherical
  */

  /**
    * Implicit conversion for [[Spherical]] coordinates into [[Cylindrical]] coordinates
    *
    * @param sphericalCoordinate The spherical coordinate
    * @return a [[Cylindrical]] coordinate
    */
  implicit def toCylindrical(sphericalCoordinate: Spherical): Cylindrical = toCylindrical(toCartesian(sphericalCoordinate))

  /**
    * Implicit conversion for [[Cylindrical]] coordinates into [[Spherical]] coordinates
    *
    * @param cylindricalCoordinate The cylindrical coordinate
    * @return a [[Spherical]] coordinate
    */
  implicit def toSpherical(cylindricalCoordinate: Cylindrical): Spherical = toSpherical(toCartesian(cylindricalCoordinate))
}

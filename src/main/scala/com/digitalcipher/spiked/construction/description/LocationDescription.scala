package com.digitalcipher.spiked.construction.description

import com.digitalcipher.spiked.topology.coords.spatial
import com.digitalcipher.spiked.topology.coords.spatial.Coordinates.{Cartesian, Cylindrical, Spherical}
import squants.space.{Angle, Length}

/**
  * The location description to move between the DNA and domain objects. See the companion object for
  * construction methods, and for converting the description back into a DNA fragment. The class is used by
  * the parser to construct coordinates, and used by the writer to persist the DNA fragments.
  * @param point A tuple representing the three dimensions for the specified coordinate type
  * @param coordinateType The coordinate type (i.e. Cartesian, cylindrical, spherical)
  */
class LocationDescription(val point: (String, String, String), val coordinateType: String) {

  import com.digitalcipher.spiked.construction.description.LocationDescription._

  private lazy val cartesianCoordinate = Cartesian((convertLength(point._1), convertLength(point._2), convertLength(point._3)))
  private lazy val cylindricalCoordinate = Cylindrical((convertLength(point._1), convertAngle(point._2), convertLength(point._3)))
  private lazy val sphericalCoordinate = Spherical((convertLength(point._1), convertAngle(point._2), convertAngle(point._3)))

  // lazily converts from the cartesian coordinate to the original coordinate type
  private lazy val coordinate = coordinateType match {
    case CARTESIAN.name => cartesianCoordinate
    case CYLINDRICAL.name => cylindricalCoordinate
    case SPHERICAL.name => sphericalCoordinate
  }

  // lazily converts from the cartesian coordinate to the original coordinate type
  private lazy val coordinateName = coordinateType match {
    case CARTESIAN.name => "cartesian"
    case CYLINDRICAL.name => "cylindrical"
    case SPHERICAL.name => "spherical"
  }

  /**
    * @return The spatial Cartesian coordinate
    */
  def cartesian: Cartesian = coordinateType match {
    case CARTESIAN.name => cartesianCoordinate
    case CYLINDRICAL.name => spatial.toCartesian(cylindricalCoordinate)
    case SPHERICAL.name => spatial.toCartesian(sphericalCoordinate)
  }

  /**
    * @return The spatial cylindrical coordinate
    */
  def cylindrical: Cylindrical = coordinateType match {
    case CARTESIAN.name => spatial.toCylindrical(cartesianCoordinate)
    case CYLINDRICAL.name => cylindricalCoordinate
    case SPHERICAL.name => spatial.toCylindrical(sphericalCoordinate)
  }

  /**
    * @return The spatial spherical coordinate
    */
  def spherical: Spherical = coordinateType match {
    case CARTESIAN.name => spatial.toSpherical(cartesianCoordinate)
    case CYLINDRICAL.name => spatial.toSpherical(cylindricalCoordinate)
    case SPHERICAL.name => sphericalCoordinate
  }

  /**
    * @return A human-readable string representation of the location description
    * @see [[com.digitalcipher.spiked.construction.description.LocationDescription.fragment]] to construct the DNA fragment for the location
    */
  override def toString: String = s"(type=$coordinateName, coordinate=${coordinate.toString})"
}

/**
  * The description holding the coordinate and coordinate type
  */
object LocationDescription {
  val LOCATION = 'LOC
  val COORDINATE_TYPE = 'cst
  val CARTESIAN = 'ct
  val CYLINDRICAL = 'cl
  val SPHERICAL = 'sp
  val PROJECTION_1 = 'px1
  val PROJECTION_2 = 'px2
  val PROJECTION_3 = 'px3

  /**
    * Converts a string representing a [[Length]] into a [[Length]].
    * @param value The string representing the length
    * @return A [[Length]]
    * @throws IllegalArgumentException if the conversion fails
    */
  def convertLength(value: String): Length = {
    val length = Length(value)
    if(length.isFailure)
      throw new IllegalArgumentException(s"Invalid length: $value")
    else
      length.get
  }

  /**
    * Converts a string representing an [[Angle]] into a [[Angle]].
    * @param value The string representing the angle
    * @return An [[Angle]]
    * @throws IllegalArgumentException if the conversion fails
    */
  def convertAngle(value: String): Angle = {
    val angle = Angle(value)
    if(angle.isFailure)
      throw new IllegalArgumentException(s"Invalid angle: $value")
    else
      angle.get
  }

  /**
    * Constructs a [[LocationDescription]] coordinate from the specified [[Cartesian]] coordinate
    *
    * @param coordinate The coordinate from which to create a [[LocationDescription]]
    * @return A [[LocationDescription]] representing the specified coordinate
    */
  def from(coordinate: Cartesian): LocationDescription = {
    val tuple = coordinate.point.tuple
    new LocationDescription((tuple._1.toString, tuple._2.toString, tuple._3.toString), CARTESIAN.name)
  }

  /**
    * Constructs a [[LocationDescription]] coordinate from the specified [[Cylindrical]] coordinate
    *
    * @param coordinate The coordinate from which to create a [[LocationDescription]]
    * @return A [[LocationDescription]] representing the specified coordinate
    */
  def from(coordinate: Cylindrical): LocationDescription = {
    val tuple = coordinate.point.tuple
    new LocationDescription((tuple._1.toString, tuple._2.toString, tuple._3.toString), CYLINDRICAL.name)
  }

  /**
    * Constructs a [[LocationDescription]] coordinate from the specified [[Spherical]] coordinate
    *
    * @param coordinate The coordinate from which to create a [[LocationDescription]]
    * @return A [[LocationDescription]] representing the specified coordinate
    */
  def from(coordinate: Spherical): LocationDescription = {
    val tuple = coordinate.point.tuple
    new LocationDescription((tuple._1.toString, tuple._2.toString, tuple._3.toString), SPHERICAL.name)
  }

  /**
    * Creates the DNA fragment representing this [[LocationDescription]]
    *
    * @param description The [[LocationDescription]] from which to create a DNA fragment
    * @return The DNA fragment representing this [[LocationDescription]]
    */
  def fragment(description: LocationDescription): String = {
    s"${LOCATION.name}=(${COORDINATE_TYPE.name}=${description.coordinateType}, " +
          s"${PROJECTION_1.name}=${description.point._1.toString}, " +
            s"${PROJECTION_2.name}=${description.point._2.toString}, " +
            s"${PROJECTION_3.name}=${description.point._3.toString}" + ")"

  }

  /**
    * Convenience function that creates a DNA fragment directly from the [[Cartesian]] coordiante
    *
    * @param coordinate The coordinate from which to create the DNA fragment
    * @return The DNA fragment representing this [[LocationDescription]]
    */
  def fragment(coordinate: Cartesian): String = fragment(LocationDescription.from(coordinate))

  /**
    * Convenience function that creates a DNA fragment directly from the [[Cylindrical]] coordiante
    *
    * @param coordinate The coordinate from which to create the DNA fragment
    * @return The DNA fragment representing this [[LocationDescription]]
    */
  def fragment(coordinate: Cylindrical): String = fragment(LocationDescription.from(coordinate))

  /**
    * Convenience function that creates a DNA fragment directly from the [[Spherical]] coordiante
    *
    * @param coordinate The coordinate from which to create the DNA fragment
    * @return The DNA fragment representing this [[LocationDescription]]
    */
  def fragment(coordinate: Spherical): String = fragment(LocationDescription.from(coordinate))
}

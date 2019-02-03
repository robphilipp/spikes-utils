package com.digitalcipher.spiked.topology.coords.dimensionless

import com.digitalcipher.spiked.topology.coords.Point

/**
  * Created by rob on 12/18/16.
  */
object Points {

  /**
    * A cartesian point
    *
    * @param tuple A 3-tuple representing the (x, y, z) coordinates
    */
  case class Cartesian(override val tuple: (Double, Double, Double)) extends Point[Double, Double, Double] {

    /**
      * Subtracts the specified point from this point.
      *
      * @param point The point to subtract from this one. Assumes the point represents a cartesian point
      * @return The (vector) difference of the two points
      */
    override def subtract(point: Point[Double, Double, Double]): Cartesian =
      Cartesian((tuple._1 - point._1, tuple._2 - point._2, tuple._3 - point._3))

    /**
      * Adds the specified point to this point.
      *
      * @param point The point to add to this one. Assumes the point represents a cartesian point
      * @return The (vector) sum of the two points
      */
    override def add(point: Point[Double, Double, Double]): Cartesian =
      Cartesian((tuple._1 + point._1, tuple._2 + point._2, tuple._3 + point._3))

    /**
      * @return A string representation of the cartesian point
      */
    override def toString: String = s"(x=${tuple._1}, y=${tuple._2}, z=${tuple._3})"
  }


  /**
    * The cylindrical point (r, φ, z), where r is the distance from the origin to the projection of the point onto the
    * plane perpendicular to the z-axis. φ is the angle r and the x-axis. And z is the height of the point above the
    * plane in which r lies.
    * Created by rob on 12/11/16.
    *
    * @param tuple A 3-tuple representing the (r, φ, z) coordinates
    */
  case class Cylindrical(override val tuple: (Double, Double, Double)) extends Point[Double, Double, Double] {

    /**
      * Subtracts the specified point from this one
      * @param point The point to subtract from this one
      * @return The (vector) difference of the two points
      */
    override def subtract(point: Point[Double, Double, Double]): Cylindrical = {
      val x = Cylindrical.toX(tuple._1, tuple._2) - Cylindrical.toX(point._1, point._2)
      val y = Cylindrical.toY(tuple._1, tuple._2) - Cylindrical.toY(point._1, point._2)
      val z = tuple._3 - point._3
      Cylindrical((Cylindrical.toR(x, y), Cylindrical.toPhi(x, y), z))
    }

    /**
      * Adds the specified point to this one
      * @param point The point to add to this one
      * @return The (vector) sum of the two points
      */
    override def add(point: Point[Double, Double, Double]): Cylindrical = {
      val x = Cylindrical.toX(tuple._1, tuple._2) + Cylindrical.toX(point._1, point._2)
      val y = Cylindrical.toY(tuple._1, tuple._2) + Cylindrical.toY(point._1, point._2)
      val z = tuple._3 + point._3
      Cylindrical((Cylindrical.toR(x, y), Cylindrical.toPhi(x, y), z))
    }

    /**
      * @return A string representation of the point
      */
    override def toString: String = s"(r=${tuple._1}, φ=${tuple._2}, z=${tuple._3})"
  }

  /**
    * A cylindrical point object that holds some basic conversion functions. For polar coordinates, just set z=0.
    */
  case object Cylindrical {
    /**
      * Calculates the cartesian x-dimension of the cylindrical point
      *
      * @param r The length of the point in the plane perpendicular to the z-axis
      * @param φ The angle of the point from the origin in the plane perpendicular to the z-axis
      * @return The cartesian x-dimension of the point
      */
    def toX(r: Double, φ: Double): Double = r * math.cos(φ)

    /**
      * Calculates the cartesian y-dimension of the cylindrical point
      *
      * @param r The length of the point in the plane perpendicular to the z-axis
      * @param φ The angle of the point from the origin in the plane perpendicular to the z-axis
      * @return The cartesian y-dimension of the point
      */
    def toY(r: Double, φ: Double): Double = r * math.sin(φ)

    /**
      * Calculates the cylindrical r-dimension from the cartesian point
      *
      * @param x The x-dimension of the cartesian point
      * @param y The y-dimension of the cartesian point
      * @return The cylindrical r-dimension of the cartesian point
      */
    def toR(x: Double, y: Double): Double = math.sqrt(x * x + y * y)

    /**
      * Calculates the angle of the point from the origin in the plane perpendicular to the z-axis
      *
      * @param x The x-dimension of the cartesian point
      * @param y The y-dimension of the cartesian point
      * @return the angle of the point from the origin in the plane perpendicular to the z-axis
      */
    def toPhi(x: Double, y: Double): Double = {
      val r = toR(x, y)
      if (x == 0 && y == 0) 0
      else if (x == 0) math.asin(y / r)
      else if (x > 0) math.atan(y / x)
      else -math.asin(y / r) + math.Pi
    }
  }

  /**
    * Spherical point that has spatial dimensions. The point (r, φ, θ) is based an standard physics usage where
    * <ul>
    *   <li>r is the radial length</li>
    *   <li>φ is the angle between the x-axis and the projection of r onto the x-y plane</li>
    *   <li>θ is the angle between the z-axis and the r</li>
    * </ul>
    * @param tuple The spherical point (r, φ, θ)
    */
  case class Spherical(override val tuple: (Double, Double, Double)) extends Point[Double, Double, Double] {

    /**
      * Adds a point to this point (vector addition) and returns the resulting point. Assumes that the points are of
      * the same type
      *
      * @param point The point to add to this one
      * @return The (vector) sum of the two points
      */
    override def add(point: Point[Double, Double, Double]): Point[Double, Double, Double] = {
      val x = Spherical.toX(tuple._1, tuple._2, tuple._3) + Spherical.toX(point.tuple._1, point.tuple._2, point.tuple._3)
      val y = Spherical.toY(tuple._1, tuple._2, tuple._3) + Spherical.toY(point.tuple._1, point.tuple._2, point.tuple._3)
      val z = Spherical.toZ(tuple._1, tuple._3) + Spherical.toZ(point.tuple._1, point.tuple._3)
      Spherical((
        Spherical.toR(x, y, z),
        Spherical.toPhi(x, y),
        Spherical.toTheta(x, y, z)
      ))
    }

    /**
      * Subtracts (vector subtraction) the specified point from this one
      *
      * @param point The point to subtract from this one
      * @return The (vector) difference of the two points
      */
    override def subtract(point: Point[Double, Double, Double]): Point[Double, Double, Double] = {
      val x = Spherical.toX(tuple._1, tuple._2, tuple._3) - Spherical.toX(point.tuple._1, point.tuple._2, point.tuple._3)
      val y = Spherical.toY(tuple._1, tuple._2, tuple._3) - Spherical.toY(point.tuple._1, point.tuple._2, point.tuple._3)
      val z = Spherical.toZ(tuple._1, tuple._3) - Spherical.toZ(point.tuple._1, point.tuple._3)
      Spherical((
        Spherical.toR(x, y, z),
        Spherical.toPhi(x, y),
        Spherical.toTheta(x, y, z)
      ))
    }

    /**
      * @return A string representation of the point
      */
    override def toString: String = s"(r=${tuple._1}, φ=${tuple._2}, θ=${tuple._3})"
  }

  /**
    * Companion object to [[Spherical]]
    */
  case object Spherical {
    /**
      * Converts the spherical coordinate to the x-dimension of a cartesian coordinate
      * @param r is the radial length
      * @param φ is the angle between the x-axis and the projection of r onto the x-y plane
      * @param θ is the angle between the z-axis and the r
      * @return The x-dimension with spatial dimensions
      */
    def toX(r: Double, φ: Double, θ: Double): Double = Cylindrical.toX(r, φ) * math.sin(θ)

    /**
      * Converts the spherical coordinate to the y-dimension of a cartesian coordinate
      * @param r is the radial length
      * @param φ is the angle between the x-axis and the projection of r onto the x-y plane
      * @param θ is the angle between the z-axis and the r
      * @return The y-dimension with spatial dimensions
      */
    def toY(r: Double, φ: Double, θ: Double): Double = Cylindrical.toY(r, φ) *  math.sin(θ)

    /**
      * Converts the spherical coordinate to the z-dimension of a cartesian coordinate
      * @param r is the radial length
      * @param θ is the angle between the z-axis and the r
      * @return The z-dimension with spatial dimensions
      */
    def toZ(r: Double, θ: Double): Double = r * math.cos(θ)

    /**
      * Converts a cartesian point to the r-dimension of a spherical coordinate
      * @param x The x-dimension of the cartesian point
      * @param y The y-dimension of the cartesian point
      * @param z The z-dimension of the cartesian point
      * @return The radial length of the spherical coordiante
      */
    def toR(x: Double, y: Double, z: Double): Double = math.sqrt( x * x + y * y + z* z )

    /**
      * Converts a cartesian point to the angle between the x-axis and the projection of r onto the x-y plane
      * @param x The x-dimension of the cartesian point
      * @param y The y-dimension of the cartesian point
      * @return the angle between the x-axis and the projection of r onto the x-y plane
      */
    def toPhi(x: Double, y: Double): Double = Cylindrical.toPhi(x, y)

    /**
      * Converts a cartesian point to the angle between the z-axis and the r
      * @param x The x-dimension of the cartesian point
      * @param y The y-dimension of the cartesian point
      * @param z The z-dimension of the cartesian point
      * @return the angle between the z-axis and the r
      */
    def toTheta(x: Double, y: Double, z: Double): Double = {
      val r = toR(x,y,z)
      if(x == 0 && y == 0) 0
      else if( z == 0 ) math.Pi / 2
      else math.acos(z / toR(x,y,z))
    }
  }
}

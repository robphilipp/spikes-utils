package com.digitalcipher.spiked.topology.coords.spatial

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.topology.coords.spatial
import com.digitalcipher.spiked.topology.coords.spatial.Coordinates.{Cartesian, Cylindrical, Spherical}
import org.scalactic.Equality
import squants.space._

/**
  * Created by rob on 12/26/16.
  */
class SpatialCoordTest extends BaseSpec {

  implicit val lengthTolerance: Length = Nanometers(1e-3)
  implicit val angleTolerance: Angle = Radians(1e-9)
  implicit val tolerance: Double = 1e-10

  implicit val cartesianEqual: Equality[Cartesian] = (a: Cartesian, b: Any) => b match {
    case coord: Cartesian => coord.point._1 =~ a.point._1 && coord.point._2 =~ a.point._2 && coord.point._3 =~ a.point._3
    case _ => false
  }

  implicit val cylindricalEqual: Equality[Cylindrical] = (a: Cylindrical, b: Any) => b match {
    case coord: Cylindrical => coord.point._1 =~ a.point._1 && coord.point._2 === a.point._2 &&coord.point._3 =~ a.point._3
    case _ => false
  }

  implicit val sphericalEqual: Equality[Spherical] = (a: Spherical, b: Any) => b match {
    case coord: Spherical => coord.point._1 =~ a.point._1 && coord.point._2 =~ a.point._2 &&coord.point._3 =~ a.point._3
    case _ => false
  }

  "A unit vector should have length 1 µm" in {
    Cartesian(1, 0, 0).norm should be (Microns(1))
    Cylindrical(1, 0, 0).norm should be (Microns(1))
    Cylindrical(0, 0, 1).norm should be (Microns(1))

    Spherical(1, 0, 0).norm should be (Microns(1))
    Spherical(1, math.Pi, 0).norm should be (Microns(1))

    Spherical(Microns(1), Radians(0), Radians(0)).norm should be (Microns(1))

    import scala.language.postfixOps
    import squants.space.LengthConversions._
    import squants.space.AngleConversions._
    Spherical(1 µm, 0 radians, 0 radians).norm should be (1 µm)
  }

  "A cartesian coordinate added to a cylindrical coordinate should yield a cartesian coordinate that is the sum of the two" in {
    Cartesian(1, 0, 0) + Cylindrical(1, math.Pi, 0) should equal(Cartesian(0, 0, 0))
    Cartesian(1, 0, 0) + Cylindrical(1, math.Pi / 2, 0) should equal(Cartesian((1, 1, 0)))
  }

  "A cylindrical coordinate added to a cartesian coordinate should yield a cylindrical coordinate that is the sum of the two" in {
    Cylindrical(1, math.Pi, 0) + Cartesian(1, 0, 0) should equal(Cylindrical(0, math.Pi / 2, 0))
    Cylindrical(1, math.Pi / 2, 0) + Cartesian(1, 0, 0) should equal(Cylindrical(math.sqrt(2), math.Pi / 4, 0))
  }

  "A cylindrical coordinate converted to a cartesian coordinate should yield a cartesian coordinate" in {
    toCartesian(Cylindrical(1, 0, 1)) should equal(Cartesian(1, 0, 1))

    import scala.language.postfixOps
    import squants.space.LengthConversions._
    import squants.space.AngleConversions._
    toCartesian(Cylindrical(1 µm, 0 radians, 1 µm)) should equal(Cartesian(1 µm, 0 µm, 1 µm))
    toCartesian(Cylindrical(1 µm, math.Pi / 2 radians, 0 µm)) should equal(Cartesian(0 µm, 1 µm, 0 µm))
  }

  "A cylindrical coordinate converted to a cartesian coordinate and then back to a cylindrical coordinate should match the original coordinate" in {
    toCylindrical(spatial.toCartesian(Cylindrical(1, 2, 3))) should equal(Cylindrical(1, 2, 3))
  }

  "A coordinate should have a distance to another coordinate" in {
    Cartesian(1, 0, 0).distanceTo(Cartesian(2, 0, 0)) should be (Microns(1))
    Cartesian(1, 0, 0).distanceTo(Cartesian(1, 0, 0)) should be (Microns(0))
    Cartesian(1, 0, 0).distanceTo(Cylindrical(2, 0, 0)) should be (Microns(1))
    Cartesian(1, 0, 0).distanceTo(Cartesian(0, 1, 0)) to Microns should equal(math.sqrt(2) +- 1e-15)
  }
}

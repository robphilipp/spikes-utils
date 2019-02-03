package com.digitalcipher.spiked.topology.coords.dimensionless

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.topology.coords.dimensionless.Coordinates.{Cartesian, Cylindrical, Spherical}
import org.scalactic.Equality

/**
  * Created by rob on 12/25/16.
  */
class DimensionlessCoordTest extends BaseSpec {

  implicit val cartesianEqual: Equality[Cartesian] = (a: Cartesian, b: Any) => b match {
    case coord: Cartesian => coord.point._1 === a.point._1 +- 1e-15 && coord.point._2 === a.point._2 +- 1e-15 && coord.point._3 === a.point._3 +- 1e-15
    case _ => false
  }

  implicit val cylindricalEqual: Equality[Cylindrical] = (a: Cylindrical, b: Any) => b match {
    case coord: Cylindrical => coord.point._1 === a.point._1 +- 1e-15 && coord.point._2 === a.point._2 +- 1e-15 && coord.point._3 === a.point._3 +- 1e-15
    case _ => false
  }

  implicit val sphericalEqual: Equality[Spherical] = (a: Spherical, b: Any) => b match {
    case coord: Spherical => coord.point._1 === a.point._1 +- 1e-15 && coord.point._2 === a.point._2 +- 1e-15 && coord.point._3 === a.point._3 +- 1e-15
    case _ => false
  }

  "A unit vector should have length 1" in {
    Cartesian(1, 0, 0).norm should be (1)
    Cylindrical(1, 0, 0).norm should be (1)
    Spherical(1, 0, 0).norm should be (1)

    Spherical(1, math.Pi, 0).norm should be (1)
  }

  "A cartesian and cylindrical unit vector should be able to be added" in {
    Cartesian(1, 0, 0) - Cylindrical(1, 0, 0) should equal(Cartesian(0, 0, 0))
    Cartesian(1, 0, 0) + Cylindrical(1, 0, 0) should equal(Cartesian(2, 0, 0))
    Cartesian(1, 0, 0) + Cylindrical(1, 0, 0) + Spherical(1, 0, 0) should equal(Cartesian(3, 0, 0))
    Cartesian(1, 0, 0) - Cylindrical(1, 0, 0) should not equal Cartesian(1, 0, 0)
    Cylindrical(1, 0, 0) - Cartesian(1, 0, 0) should equal(Cylindrical(0, 0, 0))
    Cylindrical(1, 0, 0) - Cartesian(1, 0, 0) should not equal Cylindrical(1, 0, 0)
  }
}

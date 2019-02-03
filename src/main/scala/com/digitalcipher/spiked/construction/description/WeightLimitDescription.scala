package com.digitalcipher.spiked.construction.description


/**
  * Created by rob on 1/15/17.
  */
case class WeightLimitDescription(limiterParams: WeightLimiterSpecificParams)

/**
  * Describes the weight limit functions that define the allowed ranges a weight can have
  */
object WeightLimitDescription {
  val WEIGHT_LIMITER_FUNCTION = 'WLF
  val LIMITER_TYPE = 'fnc
  val BOUNDED = 'bnd
  val UNBOUNDED = 'unb
  val LOWER_BOUND = 'lwb  // for bounded weights; lower bound of the weight
  val UPPER_BOUND = 'upb  // for bounded weights; upper bound of the weight

  /**
    * Creates the DNA fragment describing the weight-limiter function
    * @param description The weight-limiter function description
    * @return the DNA fragment describing the weight-limiter function
    */
  def fragment(description: WeightLimitDescription): String =
    s"${WEIGHT_LIMITER_FUNCTION.name}=(${description.limiterParams.fragment})"

}

/**
  * The learning-type specific parameters
  */
trait WeightLimiterSpecificParams {
  def limiterType: String
  def fragment: String
}

case class BoundedParams(lowerBound: Double, upperBound: Double) extends WeightLimiterSpecificParams {
  import com.digitalcipher.spiked.construction.description.WeightLimitDescription._
  override def limiterType: String = BOUNDED.name

  override def fragment: String = s"${LIMITER_TYPE.name}=$limiterType, " +
    s"${LOWER_BOUND.name}=$lowerBound, ${UPPER_BOUND.name}=$upperBound"
}

case class UnboundedParams() extends WeightLimiterSpecificParams {
  import com.digitalcipher.spiked.construction.description.WeightLimitDescription._
  override def limiterType: String = UNBOUNDED.name

  override def fragment: String = s"${LIMITER_TYPE.name}=$limiterType"
}

package com.digitalcipher.spiked.construction.description

import squants.Time

/**
  * Description of the synapse weight-decay functions.
  *
  * Created by rob on 1/15/17.
  */
case class WeightDecayDescription(decayParams: WeightDecaySpecificParams)

/**
  * Companion object for the weight-decay description function
  */
object WeightDecayDescription {
  val WEIGHT_DECAY_FUNCTION = 'WDF
  val DECAY_TYPE = 'fnc
  val EXPONENTIAL = 'exp
  val ZERO = 'zer
  val DECAY_HALF_LIFE = 'dhl

  /**
    * Creates the DNA fragment describing the weight-decay function
    * @param description The weight-decay function description
    * @return the DNA fragment describing the weight-decay function
    */
  def fragment(description: WeightDecayDescription): String =
    s"${WEIGHT_DECAY_FUNCTION.name}=(${description.decayParams.fragment})"
}

/**
  * The learning-type specific parameters
  */
trait WeightDecaySpecificParams {
  def decayType: String
  def fragment: String
}

/**
  * The parameters for an exponential weight-decay function
  * @param decayHalfLife The half-life of the weight decay
  */
case class ExponentialDecayParams(decayHalfLife: Time) extends WeightDecaySpecificParams {
  import com.digitalcipher.spiked.construction.description.WeightDecayDescription._
  override def decayType: String = EXPONENTIAL.name

  override def fragment: String = s"${DECAY_TYPE.name}=$decayType, ${DECAY_HALF_LIFE.name}=$decayHalfLife"
}

/**
  * The parameters for the no-decay weight decay function (i.e. no weight decay)
  */
case class NoDecayParams() extends WeightDecaySpecificParams {
  import com.digitalcipher.spiked.construction.description.WeightDecayDescription._
  override def decayType: String = ZERO.name

  override def fragment: String = s"${DECAY_TYPE.name}=$decayType"
}

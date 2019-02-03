package com.digitalcipher.spiked.construction.description

import squants.Time

/**
  * Created by rob on 1/15/17.
  */
case class LearningFunctionDescription(learningParams: LearningSpecificParams)

/**
  * The learning function description support object
  */
object LearningFunctionDescription {
  // LRN=[(fnc=stdp, ina=0.035, inp=3.0, exa=0.035, exp=3.0), (fnc=flat)]
  val LEARNING_FUNCTIONS = 'LRN
  val LEARNING_TYPE = 'fnc

  val STDP_HARD = 'stdp_hard
  val STDP_SOFT = 'stdp_soft
  val INHIBITION_AMPLITUDE = 'ina
  val INHIBITION_PERIOD = 'inp
  val EXCITATION_AMPLITUDE = 'exa
  val EXCITATION_PERIOD = 'exp

  val STDP_ALPHA = 'stdp_alpha
  val ALPHA_BASELINE = 'bln
  val ALPHA_LEARNING_RATE = 'alr
  val ALPHA_TIME_CONSTANT = 'atc

  val NO_LEARNING = 'flat

  /**
    * Creates the DNA fragment describing the spike-timing-dependent plasticity function
    * @param description The spike-timing-dependent plasticity function description
    * @return the DNA fragment describing the spike-timing-dependent plasticity function
    */
  def fragment(description: LearningFunctionDescription): String = s"(${description.learningParams.fragment})"

  /**
    * Creates DNA fragment for the collection of the learning functions
    * @param descriptions The collection of learning function descriptions
    * @return DNA fragment for the collection of the learning functions
    */
  def fragment(descriptions: Map[String, LearningFunctionDescription]): String =
    s"${LEARNING_FUNCTIONS.name}=[${descriptions.map(entry => fragment(entry._2)).mkString(", ")}]"
}

/**
  * The learning-type specific parameters
  */
trait LearningSpecificParams {
  def learningType: String
  def fragment: String
}

/**
  * The offset approximation to the STDP learning method
  * @param inhibitionAmplitude The inhibition amplitude
  * @param inhibitionPeriod The inhibition period
  * @param excitationAmplitude The excitation amplitude
  * @param excitationPeriod The excitation period
  */
case class StdpHardLimitLearningParams(inhibitionAmplitude: Double,
                                       inhibitionPeriod: Time,
                                       excitationAmplitude: Double,
                                       excitationPeriod: Time) extends LearningSpecificParams {

  import com.digitalcipher.spiked.construction.description.LearningFunctionDescription._
  override val learningType: String = STDP_HARD.name

  /**
    * @return The DNA fragment representing the offset approximation to the STDP learning approach
    */
  override def fragment: String = s"${LEARNING_TYPE.name}=$learningType, " +
    s"${INHIBITION_AMPLITUDE.name}=$inhibitionAmplitude, ${INHIBITION_PERIOD.name}=$inhibitionPeriod, " +
    s"${EXCITATION_AMPLITUDE.name}=$excitationAmplitude, ${EXCITATION_PERIOD.name}=$excitationPeriod"
}

/**
  * The offset approximation to the STDP learning method
  * @param inhibitionAmplitude The inhibition amplitude
  * @param inhibitionPeriod The inhibition period
  * @param excitationAmplitude The excitation amplitude
  * @param excitationPeriod The excitation period
  */
case class StdpSoftLimitLearningParams(inhibitionAmplitude: Double,
                                       inhibitionPeriod: Time,
                                       excitationAmplitude: Double,
                                       excitationPeriod: Time) extends LearningSpecificParams {

  import com.digitalcipher.spiked.construction.description.LearningFunctionDescription._
  override val learningType: String = STDP_SOFT.name

  /**
    * @return The DNA fragment representing the offset approximation to the STDP learning approach
    */
  override def fragment: String = s"${LEARNING_TYPE.name}=$learningType, " +
    s"${INHIBITION_AMPLITUDE.name}=$inhibitionAmplitude, ${INHIBITION_PERIOD.name}=$inhibitionPeriod, " +
    s"${EXCITATION_AMPLITUDE.name}=$excitationAmplitude, ${EXCITATION_PERIOD.name}=$excitationPeriod"
}

/**
  * Alpha-function learning rule
  * @param baseline The inhibition weight change magnitude
  * @param timeConstant The time-constant for the rise and decay of the excitation weight change magnitude
  * @param learningRate The learning rate, multiplies the weight change
  */
case class StdpAlphaLearningParams(baseline: Double, timeConstant: Time, learningRate: Double) extends LearningSpecificParams {
  import com.digitalcipher.spiked.construction.description.LearningFunctionDescription._
  override val learningType: String = STDP_ALPHA.name

  /**
    * @return The DNA fragment representing the alpha-function STDP learning rule
    */
  override def fragment: String = s"${LEARNING_TYPE.name}=$learningType, " +
    s"${ALPHA_BASELINE.name}=$baseline, ${ALPHA_TIME_CONSTANT.name}=$timeConstant, ${ALPHA_LEARNING_RATE.name}=$learningRate"
}

/**
  * No STDP learning applied.
  */
case class NoLearningParams() extends LearningSpecificParams {
  import com.digitalcipher.spiked.construction.description.LearningFunctionDescription._
  override val learningType: String = NO_LEARNING.name

  override def fragment: String = s"${LEARNING_TYPE.name}=$learningType"
}

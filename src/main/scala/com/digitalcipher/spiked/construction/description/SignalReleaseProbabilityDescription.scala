package com.digitalcipher.spiked.construction.description

import squants.Time

/**
  * Description of Hebbian learning function that accounts facilitation and depletion. If a pre-synaptic
  * synapse receives many spikes, calcium concentrations increase, and firing can be easier. Once
  * the neuron fires, some of the neuron-transmitter vesicles are depleted, and that reduces the
  * probability of firing.
  * @param facilitator The facilitation parameters
  * @param depletion The depletion parameters
  * @see code for the signal release probability for further details
  */
case class SignalReleaseProbabilityDescription(facilitator: SignalTimingFunction, depletion: SignalTimingFunction)

/**
  * Holds elements needed for description and for generating dna fragments
  */
object SignalReleaseProbabilityDescription {
  val SIGNAL_RELEASE_PROBABILITY = 'SRP
  val FACILITATION_BASE = 'fcb
  val FACILITATION_MAGNITUDE = 'fcm
  val FACILITATION_TIME_CONSTANT = 'fct
  val DEPLETION_BASE = 'dpb
  val DEPLETION_MAGNITUDE = 'dpm
  val DEPLETION_TIME_CONSTANT = 'dpt

  /**
    * @param description The synapse timing description
    * @return The DNA fragment for the synapse timing
    */
  def fragment(description: SignalReleaseProbabilityDescription): String = {
    s"${SIGNAL_RELEASE_PROBABILITY.name}=(" +
      s"${FACILITATION_BASE.name}=${description.facilitator.base}," +
      s"${FACILITATION_MAGNITUDE.name}=${description.facilitator.magnitude}," +
      s"${FACILITATION_TIME_CONSTANT.name}=${description.facilitator.timeConstant}," +
      s"${DEPLETION_BASE.name}=${description.depletion.base}," +
      s"${DEPLETION_MAGNITUDE.name}=${description.depletion.magnitude}," +
      s"${DEPLETION_TIME_CONSTANT.name}=${description.depletion.timeConstant}" +
      s")"
  }
}

/**
  * The synapse timing function is composed of a facilitator and a depletion function that both have
  * the same sets of parameters.
  * <p>
  * f(t; b, m, t,,c,,)
  * </p>
  *
  * @param base The base value at t = 0
  * @param magnitude The magnitude of the change
  * @param timeConstant The decay time-constant
  */
case class SignalTimingFunction(base: Double, magnitude: Double, timeConstant: Time)
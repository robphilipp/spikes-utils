package com.digitalcipher.spiked.construction.description

import squants.electro.{ElectricPotential, MagneticFlux}
import squants.time.Frequency
import squants.{Time, Velocity}

/**
  *
  * {{{NRN=(
  * nid=<int>;
  * nty=<mi|bi|mr|br>;
  * inh=<t|f>;  // inhibition neuron (t,f); t=true, f=false
  * LOC=(cst=<ct|cl|sp>; px1=<double>;px2=<double>;px3=<double>);
  * rfp=<double>;  // refractory period
  * rfb=<double>; // base-refractoriness (units of magnetic flux are Webers (V•s) 1 V•s = 1e6 mV•ms, typical units in µWb or nWb)
  * mnp=<double>;  // minimum allowed membrane potential
  * mpd=<double>;  // membrane potential decay half-life
  * WDF=(fnc=<string>;fp1=<any>;...fpN=<any>);
  * WLF=(fnc=<string>;fp1=<any>;...fpN=<any>);
  * SNT=(fcb=<double>, fcm=<double>, fct=<Time>, dpb=<double>, dpm=<double>, dpt=<Time>),
  * scs=<double>;
  * mst=<double>;  // mono-int only
  * lct=<double>;  // bi-it only
  * rst=<double>;  // bi-it only
  * tfr=<double>   // bi-it only
  * )}}}
  * Created by rob on 1/16/17.
  */
case class NeuronDescription(neuronId: String,
                             groupId: String,
                             inhibitor: Boolean,
                             refractoryPeriod: Time,
                             baseRefractoriness: MagneticFlux,
                             minMembranePotential: ElectricPotential,
                             membranePotentialDecayHalfLife: Time,
                             membranePotentialRiseHalfLife: Time,
                             membranePotentialNoise: ElectricPotential,
                             weightNoiseMagnitude: Double,
                             spikePotential: ElectricPotential,
                             conductanceSpeed: Velocity,
                             intrinsicPlasticityBase: ElectricPotential,
                             intrinsicPlasticityLearningRate: ElectricPotential,
                             intrinsicPlasticityDecayHalfLife: Time,
                             neuronSpecificParams: NeuronSpecificParams,
                             weightDecayDescription: WeightDecayDescription,
                             weightLimitDescription: WeightLimitDescription,
                             synapseTimingDescription: SignalReleaseProbabilityDescription,
                             locationDescription: LocationDescription)

/**
  * Companion object for neuron description
  */
object NeuronDescription {

  val NEURONS = 'NRN
  val NEURON_ID = 'nid

  val NEURON_GROUP = 'grp

  val NEURON_TYPE = 'nty
  val MONOSTABLE_INTEGRATOR = 'mi
  val BISTABLE_INTEGRATOR = 'bi
  //  val MONOSTABLE_RESONATOR = 'mr
  //  val BISTABLE_RESONATOR = 'br

  val INHIBITION = 'inh
  val INHIBITION_NEURON = 't
  val EXCITATION_NEURON = 'f

  val REFRACTORY_PERIOD = 'rfp
  val BASE_REFRACTORINESS = 'rfb
  val MIN_MEMBRANE_POTENTIAL = 'mnp
  val MEMBRANE_POTENTIAL_DECAY = 'mpd
  val MEMBRANE_POTENTIAL_RISE = 'mpr
  val MEMBRANE_POTENTIAL_NOISE = 'mpn
  val WEIGHT_NOISE = 'wnm
  val SPIKE_POTENTIAL = 'spp
  val CONDUCTANCE_SPEED = 'csp
  val INTRINSIC_PLASTICITY_BASE = 'ipb
  val INTRINSIC_PLASTICITY_LEARNING_RATE = 'ipl
  val INTRINSIC_PLASTICITY_DECAY_HALF_LIFE = 'ipd

  def fragment(neuronDescription: NeuronDescription): String = {
    s"(${NEURON_ID.name}=${neuronDescription.neuronId}, " +
      s"${NEURON_GROUP.name}=${neuronDescription.groupId}, " +
      s"${neuronDescription.neuronSpecificParams.fragment}, " +
      s"${INHIBITION.name}=${if (neuronDescription.inhibitor) INHIBITION_NEURON.name else EXCITATION_NEURON.name}, " +
      s"${REFRACTORY_PERIOD.name}=${neuronDescription.refractoryPeriod}, " +
      s"${BASE_REFRACTORINESS.name}=${neuronDescription.baseRefractoriness}, " +
      s"${MIN_MEMBRANE_POTENTIAL.name}=${neuronDescription.minMembranePotential}, " +
      s"${MEMBRANE_POTENTIAL_DECAY.name}=${neuronDescription.membranePotentialDecayHalfLife}, " +
      s"${MEMBRANE_POTENTIAL_RISE.name}=${neuronDescription.membranePotentialRiseHalfLife}, " +
      s"${MEMBRANE_POTENTIAL_NOISE.name}=${neuronDescription.membranePotentialNoise}, " +
      s"${WEIGHT_NOISE.name}=${neuronDescription.weightNoiseMagnitude}, " +
      s"${SPIKE_POTENTIAL.name}=${neuronDescription.spikePotential}, " +
      s"${CONDUCTANCE_SPEED.name}=${neuronDescription.conductanceSpeed}, " +
      s"${INTRINSIC_PLASTICITY_BASE.name}=${neuronDescription.intrinsicPlasticityBase}, " +
      s"${INTRINSIC_PLASTICITY_LEARNING_RATE.name}=${neuronDescription.intrinsicPlasticityLearningRate}, " +
      s"${INTRINSIC_PLASTICITY_DECAY_HALF_LIFE.name}=${neuronDescription.intrinsicPlasticityDecayHalfLife}, " +
      s"${WeightDecayDescription.fragment(neuronDescription.weightDecayDescription)}, " +
      s"${WeightLimitDescription.fragment(neuronDescription.weightLimitDescription)}, " +
      s"${SignalReleaseProbabilityDescription.fragment(neuronDescription.synapseTimingDescription)}," +
      s"${LocationDescription.fragment(neuronDescription.locationDescription)}" +
      s")"
  }

  /**
    * Creates DNA fragment for the collection of the neurons
    *
    * @param descriptions The collection of neuron descriptions
    * @return DNA fragment for the collection of the neurons
    */
  def fragment(descriptions: Map[String, NeuronDescription]): String =
    s"${NEURONS.name}=[${descriptions.map(entry => fragment(entry._2)).mkString(", ")}]"
}

trait NeuronSpecificParams {
  def fragment: String
}

/**
  * Specific parameters for the monostable integrator neuron
  *
  * @param spikeThreshold membrane potential (mV) threshold for spiking
  */
case class MonostableIntegratorParams(spikeThreshold: ElectricPotential) extends NeuronSpecificParams {

  import MonostableIntegratorParams._
  import NeuronDescription._

  override def fragment: String = s"${NEURON_TYPE.name}=${MONOSTABLE_INTEGRATOR.name}, " +
    s"${SPIKE_THRESHOLD.name}=$spikeThreshold"
}

case object MonostableIntegratorParams {
  val SPIKE_THRESHOLD = 'mst
}

/**
  * Specific parameters for the bistable integrator neuron
  *
  * @param limitCycleThreshold   membrane potential (mV) threshold for transition to limit cycle
  * @param restingStateThreshold membrane potential (mV) threshold for transition to resting
  * @param tonicFireRate         tonic firing rate (Hz) while in limit cycle
  */
case class BistableIntegratorParams(limitCycleThreshold: ElectricPotential,
                                    restingStateThreshold: ElectricPotential,
                                    tonicFireRate: Frequency) extends NeuronSpecificParams {

  import BistableIntegratorParams._
  import NeuronDescription._

  override def fragment: String = s"${NEURON_TYPE.name}=${BISTABLE_INTEGRATOR.name}, " +
    s"${LIMIT_CYCLE_THRESHOLD.name}=$limitCycleThreshold, " +
    s"${RESTING_STATE_THRESHOLD.name}=$restingStateThreshold, " +
    s"${TONIC_FIRE_RATE.name}=$tonicFireRate"
}

case object BistableIntegratorParams {
  val LIMIT_CYCLE_THRESHOLD = 'lct
  val RESTING_STATE_THRESHOLD = 'rst
  val TONIC_FIRE_RATE = 'tfr
}
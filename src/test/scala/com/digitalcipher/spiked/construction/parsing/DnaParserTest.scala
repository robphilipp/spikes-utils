package com.digitalcipher.spiked.construction.parsing

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.construction.description.LocationDescription.CARTESIAN
import com.digitalcipher.spiked.construction.description.{NeuronDescription, _}
import squants.electro.{Millivolts, Webers}
import squants.motion.MetersPerSecond
import squants.time.{Milliseconds, Seconds}

class DnaParserTest extends BaseSpec {

  val parser = DnaParser()

  "for a valid network description file, a DNA parser" must {

    "have the six described neurons" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())
      val description = result.right.get
      val actualNeuronDescriptions: Map[String, NeuronDescription] = description.neurons

      // there should be 6 neurons
      actualNeuronDescriptions.size should be(6)

      // the neuron IDs should match the file
      actualNeuronDescriptions.keySet == Set("in-1", "in-2", "inh-1", "inh-2", "out-1", "out-2")
    }

    "parse without errors" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())

      // there should be no parsing errors
      result.isLeft should be(false)
      result.isRight should be(true)
    }

    "reparse the fragment into a matching description" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())
      val expectedNeuronDescriptions = neuronDescriptions()

      val description = result.right.get
      val actualNeuronDescriptions: Map[String, NeuronDescription] = description.neurons

      // create a dna fragment from the network description
      val dna: String = NetworkDescription.fragment(description)

      // parse the dna fragment, and grab the fragment from the newly created description.
      // this newly create fragment should match the original fragment
      parser.parseDna(dna)
        .map(description => NetworkDescription.fragment(description))
        .getOrElse("failed to reparse") should be(dna)
    }

    "have the correct group description" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())
      val description = result.right.get
      val groups: Map[String, GroupDescription] = description.groups

      // there should two groups, one with group ID "group1" and the other with group ID "group2"
      groups.size should be(2)

      // the keys should have the two group IDs
      groups.keySet == Set("group1", "group2")

      // group 1 should be a local network group
      groups("group1").isInstanceOf[GroupDescription]
      groups("group1").groupId should be("group1")
      groups("group1").params.isInstanceOf[LocalGroupParams]

      // group 2 should be a remote network group
      groups("group2").isInstanceOf[GroupDescription]
      groups("group2").groupId should be("group2")
      groups("group2").params.isInstanceOf[RemoteGroupParams]
      groups("group2").params.asInstanceOf[RemoteGroupParams].host should be("192.168.1.174")
      groups("group2").params.asInstanceOf[RemoteGroupParams].port should be(2552)
    }

    "have the parsed neuron descriptions matching the expected neuron descriptions" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())
      val expectedNeuronDescriptions = neuronDescriptions()

      val description = result.right.get
      val actualNeuronDescriptions: Map[String, NeuronDescription] = description.neurons

      // explicit checks because fragments (strings) don't always match in format
      expectedNeuronDescriptions.foreach({ case (id, expected) =>
        val actual = actualNeuronDescriptions(id)

        // neuron characteristics
        actual.neuronId should be(expected.neuronId)
        actual.groupId should be(expected.groupId)
        actual.neuronSpecificParams.getClass should be(expected.neuronSpecificParams.getClass)
        actual.neuronSpecificParams.fragment should be(expected.neuronSpecificParams.fragment)
        actual.inhibitor should be(expected.inhibitor)

        // refractoriness
        actual.refractoryPeriod should be(expected.refractoryPeriod)
        actual.baseRefractoriness should be(expected.baseRefractoriness)

        // membrane potential dynamics
        actual.minMembranePotential should be(expected.minMembranePotential)
        actual.membranePotentialDecayHalfLife should be(expected.membranePotentialDecayHalfLife)
        actual.membranePotentialRiseHalfLife should be(expected.membranePotentialRiseHalfLife)
        actual.membranePotentialNoise should be(expected.membranePotentialNoise)

        // weights
        actual.weightNoiseMagnitude should be(expected.weightNoiseMagnitude)
        WeightDecayDescription.fragment(actual.weightDecayDescription) should be(WeightDecayDescription.fragment(expected.weightDecayDescription))
        WeightLimitDescription.fragment(actual.weightLimitDescription) should be(WeightLimitDescription.fragment(expected.weightLimitDescription))

        // spikes
        actual.spikePotential should be(expected.spikePotential)
        actual.conductanceSpeed should be(expected.conductanceSpeed)

        // intrinsic plasticity
        actual.intrinsicPlasticityBase should be(expected.intrinsicPlasticityBase)
        actual.intrinsicPlasticityDecayHalfLife should be(expected.intrinsicPlasticityDecayHalfLife)
        actual.intrinsicPlasticityLearningRate should be(expected.intrinsicPlasticityLearningRate)

        // signal release probability
        actual.synapseTimingDescription.depletion.base should be(expected.synapseTimingDescription.depletion.base)
        actual.synapseTimingDescription.depletion.magnitude should be(expected.synapseTimingDescription.depletion.magnitude)
        actual.synapseTimingDescription.depletion.timeConstant should be(expected.synapseTimingDescription.depletion.timeConstant)
        actual.synapseTimingDescription.facilitator.base should be(expected.synapseTimingDescription.facilitator.base)
        actual.synapseTimingDescription.facilitator.magnitude should be(expected.synapseTimingDescription.facilitator.magnitude)
        actual.synapseTimingDescription.facilitator.timeConstant should be(expected.synapseTimingDescription.facilitator.timeConstant)

        actual.locationDescription.toString should be(expected.locationDescription.toString)
      })
    }

    "have have 4 connections from input to output layer, 2 connections from inhibition to output layer, and 2 from output to inhibition layer connections" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())
      val description = result.right.get

      val actualConnections: Map[(String, String), ConnectionDescription] = description.connections
        .map(description => (description.preSynapticNeuronId, description.postSynapticNeuronId) -> description)
        .toMap

      val expectedConnections = connectionsDescriptions()

      actualConnections.size should be(expectedConnections.size)

      // all the expected connections should be present in the actual connections, and the
      // individual connections should be the same
      expectedConnections.foreach({case (prePost, expected) =>
          actualConnections(prePost) should be(expected)
      })
    }

    "have all the learning functions defined in the description" in {
      val result: Either[Seq[String], NetworkDescription] = parser.parseDna(wtaNetworkDna())
      val description = result.right.get

      val actualLearningFunctions: Map[String, LearningFunctionDescription] = description.learningFunctions

      val expectedLearningFunctions = learningFunctionDescriptions()

      actualLearningFunctions.size should be(expectedLearningFunctions.size)

      expectedLearningFunctions.foreach({case (name, expected) =>
          actualLearningFunctions(name) should be(expected)
      })
    }
  }

  "for an invalid network description file, a DNA parser" must {

    "fail when attempting to parse a description where connections have an invalid parameter" in {
     val result: Either[Seq[String], NetworkDescription] = parser.parseDna(invalidNetwork_extraCommaInConnections())
      result.isLeft should be(true)
//      result.isRight should be(true)

    }
  }

  /**
    * Creates the expected neuron descriptions
    *
    * @return A `map(neuron_id -> neuron_description)` holding the expected neuron descriptions
    */
  def neuronDescriptions(): Map[String, NeuronDescription] = {

    /*
    // input layer
    (nid=in-1, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=-300 µm, px2=0µm, px3=100 µm)
    )
     */
    val inputNeuron1 = NeuronDescription(
      neuronId = "in-1",
      groupId = "group1",
      neuronSpecificParams = MonostableIntegratorParams(spikeThreshold = Millivolts(1)),
      inhibitor = false,

      refractoryPeriod = Milliseconds(2), baseRefractoriness = Webers(1e-7),

      minMembranePotential = Millivolts(0),
      membranePotentialDecayHalfLife = Milliseconds(2500), membranePotentialRiseHalfLife = Milliseconds(2),
      membranePotentialNoise = Millivolts(0),

      spikePotential = Millivolts(1.1),

      conductanceSpeed = MetersPerSecond(0.1),

      intrinsicPlasticityBase = Millivolts(0), intrinsicPlasticityLearningRate = Millivolts(0), intrinsicPlasticityDecayHalfLife = Seconds(3600),

      weightNoiseMagnitude = 0,
      weightDecayDescription = WeightDecayDescription(NoDecayParams()),
      weightLimitDescription = WeightLimitDescription(BoundedParams(lowerBound = 0, upperBound = 1)),

      synapseTimingDescription = SignalReleaseProbabilityDescription(
        facilitator = SignalTimingFunction(base = 1000, magnitude = 0.1, timeConstant = Milliseconds(100)),
        depletion = SignalTimingFunction(base = 1000, magnitude = 0.1, timeConstant = Milliseconds(100))
      ),

      locationDescription = new LocationDescription(point = ("-300 µm", "0 µm", "100 µm"), coordinateType = CARTESIAN.name)
    )

    /*
    (nid=in-2, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=100 µm)
    )
     */
    val inputNeuron2 = NeuronDescription(
      neuronId = "in-2",
      groupId = "group1",
      neuronSpecificParams = MonostableIntegratorParams(spikeThreshold = Millivolts(1)),
      inhibitor = false,

      refractoryPeriod = Milliseconds(2), baseRefractoriness = Webers(1e-7),

      minMembranePotential = Millivolts(0),
      membranePotentialDecayHalfLife = Milliseconds(2500), membranePotentialRiseHalfLife = Milliseconds(2),
      membranePotentialNoise = Millivolts(0),

      spikePotential = Millivolts(1.1),

      conductanceSpeed = MetersPerSecond(0.1),

      intrinsicPlasticityBase = Millivolts(0), intrinsicPlasticityLearningRate = Millivolts(0), intrinsicPlasticityDecayHalfLife = Seconds(3600),

      weightNoiseMagnitude = 0,
      weightDecayDescription = WeightDecayDescription(NoDecayParams()),
      weightLimitDescription = WeightLimitDescription(BoundedParams(lowerBound = 0, upperBound = 1)),

      synapseTimingDescription = SignalReleaseProbabilityDescription(
        facilitator = SignalTimingFunction(base = 1000, magnitude = 0.1, timeConstant = Milliseconds(100)),
        depletion = SignalTimingFunction(base = 1000, magnitude = 0.1, timeConstant = Milliseconds(100))
      ),

      locationDescription = new LocationDescription(point = ("300 µm", "0 µm", "100 µm"), coordinateType = CARTESIAN.name)
    )

    /*
    (nid=inh-1, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=exp, dhl=10 s),
        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        LOC=(cst=ct, px1=-290 µm, px2=0 µm, px3=0 µm)
    )
     */
    val inhibNeuron1 = NeuronDescription(
      neuronId = "inh-1",
      groupId = "group1",
      neuronSpecificParams = MonostableIntegratorParams(spikeThreshold = Millivolts(0.4)),
      inhibitor = true,

      refractoryPeriod = Milliseconds(0.1), baseRefractoriness = Webers(1e-7),

      minMembranePotential = Millivolts(0),
      membranePotentialDecayHalfLife = Milliseconds(250), membranePotentialRiseHalfLife = Milliseconds(2),
      membranePotentialNoise = Millivolts(0),

      spikePotential = Millivolts(0.5),

      conductanceSpeed = MetersPerSecond(0.08),

      intrinsicPlasticityBase = Millivolts(0), intrinsicPlasticityLearningRate = Millivolts(0), intrinsicPlasticityDecayHalfLife = Seconds(3600),

      weightNoiseMagnitude = 0,
      weightDecayDescription = WeightDecayDescription(ExponentialDecayParams(decayHalfLife = Seconds(10))),
      weightLimitDescription = WeightLimitDescription(BoundedParams(lowerBound = 0, upperBound = 1.5)),

      synapseTimingDescription = SignalReleaseProbabilityDescription(
        facilitator = SignalTimingFunction(base = 1000, magnitude = 0, timeConstant = Milliseconds(100)),
        depletion = SignalTimingFunction(base = 1000, magnitude = 0, timeConstant = Milliseconds(100))
      ),

      locationDescription = new LocationDescription(point = ("-290 µm", "0 µm", "0 µm"), coordinateType = CARTESIAN.name)
    )

    /*
    (nid=inh-2, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=exp, dhl=10 s),
        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        LOC=(cst=ct, px1=290 µm, px2=0 µm, px3=0 µm)
    )
     */
    val inhibNeuron2 = NeuronDescription(
      neuronId = "inh-2",
      groupId = "group1",
      neuronSpecificParams = MonostableIntegratorParams(spikeThreshold = Millivolts(0.4)),
      inhibitor = true,

      refractoryPeriod = Milliseconds(0.1), baseRefractoriness = Webers(1e-7),

      minMembranePotential = Millivolts(0),
      membranePotentialDecayHalfLife = Milliseconds(250), membranePotentialRiseHalfLife = Milliseconds(2),
      membranePotentialNoise = Millivolts(0),

      spikePotential = Millivolts(0.5),

      conductanceSpeed = MetersPerSecond(0.08),

      intrinsicPlasticityBase = Millivolts(0), intrinsicPlasticityLearningRate = Millivolts(0), intrinsicPlasticityDecayHalfLife = Seconds(3600),

      weightNoiseMagnitude = 0,
      weightDecayDescription = WeightDecayDescription(ExponentialDecayParams(decayHalfLife = Seconds(10))),
      weightLimitDescription = WeightLimitDescription(BoundedParams(lowerBound = 0, upperBound = 1.5)),

      synapseTimingDescription = SignalReleaseProbabilityDescription(
        facilitator = SignalTimingFunction(base = 1000, magnitude = 0, timeConstant = Milliseconds(100)),
        depletion = SignalTimingFunction(base = 1000, magnitude = 0, timeConstant = Milliseconds(100))
      ),

      locationDescription = new LocationDescription(point = ("290 µm", "0 µm", "0 µm"), coordinateType = CARTESIAN.name)
    )

    /*
    (nid=out-1, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=-300 µm, px2=0 µm, px3=0 µm)
    )
     */
    val outputNeuron1 = NeuronDescription(
      neuronId = "out-1",
      groupId = "group1",
      neuronSpecificParams = MonostableIntegratorParams(spikeThreshold = Millivolts(1)),
      inhibitor = false,

      refractoryPeriod = Milliseconds(20), baseRefractoriness = Webers(1e-7),

      minMembranePotential = Millivolts(0),
      membranePotentialDecayHalfLife = Milliseconds(2500), membranePotentialRiseHalfLife = Milliseconds(2),
      membranePotentialNoise = Millivolts(0),

      spikePotential = Millivolts(1),

      conductanceSpeed = MetersPerSecond(1),

      intrinsicPlasticityBase = Millivolts(0), intrinsicPlasticityLearningRate = Millivolts(0), intrinsicPlasticityDecayHalfLife = Seconds(3600),

      weightNoiseMagnitude = 1e-5,
      weightDecayDescription = WeightDecayDescription(NoDecayParams()),
      weightLimitDescription = WeightLimitDescription(BoundedParams(lowerBound = 0, upperBound = 1)),

      synapseTimingDescription = SignalReleaseProbabilityDescription(
        facilitator = SignalTimingFunction(base = 1000, magnitude = 0.1, timeConstant = Milliseconds(100)),
        depletion = SignalTimingFunction(base = 1000, magnitude = 10, timeConstant = Milliseconds(100))
      ),

      locationDescription = new LocationDescription(point = ("-300 µm", "0 µm", "0 µm"), coordinateType = CARTESIAN.name)
    )

    /*
    (nid=out-2, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=0 µm)
    )
     */
    val outputNeuron2 = NeuronDescription(
      neuronId = "out-2",
      groupId = "group1",
      neuronSpecificParams = MonostableIntegratorParams(spikeThreshold = Millivolts(1)),
      inhibitor = false,

      refractoryPeriod = Milliseconds(20), baseRefractoriness = Webers(1e-7),

      minMembranePotential = Millivolts(0),
      membranePotentialDecayHalfLife = Milliseconds(2500), membranePotentialRiseHalfLife = Milliseconds(2),
      membranePotentialNoise = Millivolts(0),

      spikePotential = Millivolts(1),

      conductanceSpeed = MetersPerSecond(1),

      intrinsicPlasticityBase = Millivolts(0), intrinsicPlasticityLearningRate = Millivolts(0), intrinsicPlasticityDecayHalfLife = Seconds(3600),

      weightNoiseMagnitude = 1e-5,
      weightDecayDescription = WeightDecayDescription(NoDecayParams()),
      weightLimitDescription = WeightLimitDescription(BoundedParams(lowerBound = 0, upperBound = 1)),

      synapseTimingDescription = SignalReleaseProbabilityDescription(
        facilitator = SignalTimingFunction(base = 1000, magnitude = 0.1, timeConstant = Milliseconds(100)),
        depletion = SignalTimingFunction(base = 1000, magnitude = 10, timeConstant = Milliseconds(100))
      ),

      locationDescription = new LocationDescription(point = ("300 µm", "0 µm", "0 µm"), coordinateType = CARTESIAN.name)
    )

    Map(
      "in-1" -> inputNeuron1, "in-2" -> inputNeuron2,
      "inh-1" -> inhibNeuron1, "inh-2" -> inhibNeuron2,
      "out-1" -> outputNeuron1, "out-2" -> outputNeuron2
    )
  }

  /**
    * @return A `map((pre, post) -> description)` holding the pre and post synaptic neuron IDs and their
    *         associated connection description
    */
  def connectionsDescriptions(): Map[(String, String), ConnectionDescription] = {
    Map(
      // (prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_alpha),
      ("in-1", "out-1") -> ConnectionDescription(
        preSynapticNeuronId = "in-1",
        postSynapticNeuronId = "out-1",
        initialWeight = 0.5,
        equilibriumWeight = 0.5,
        learningFunctionName = "stdp_alpha"
      ),
      ("in-1", "out-2") -> ConnectionDescription(
        preSynapticNeuronId = "in-1",
        postSynapticNeuronId = "out-2",
        initialWeight = 0.5,
        equilibriumWeight = 0.5,
        learningFunctionName = "stdp_alpha"
      ),
      ("in-2", "out-1") -> ConnectionDescription(
        preSynapticNeuronId = "in-2",
        postSynapticNeuronId = "out-1",
        initialWeight = 0.5,
        equilibriumWeight = 0.5,
        learningFunctionName = "stdp_alpha"
      ),
      ("in-2", "out-2") -> ConnectionDescription(
        preSynapticNeuronId = "in-2",
        postSynapticNeuronId = "out-2",
        initialWeight = 0.5,
        equilibriumWeight = 0.5,
        learningFunctionName = "stdp_alpha"
      ),

      // (prn=out-1, psn=inh-1, cnw=1, eqw=1, lrn=flat),
      // (prn=out-2, psn=inh-2, cnw=1, eqw=1, lrn=flat),
      ("out-1", "inh-1") -> ConnectionDescription(
        preSynapticNeuronId = "out-1",
        postSynapticNeuronId = "inh-1",
        initialWeight = 1,
        equilibriumWeight = 1,
        learningFunctionName = "flat"
      ),
      ("out-2", "inh-2") -> ConnectionDescription(
        preSynapticNeuronId = "out-2",
        postSynapticNeuronId = "inh-2",
        initialWeight = 1,
        equilibriumWeight = 1,
        learningFunctionName = "flat"
      ),

      // (prn=inh-1, psn=out-2, cnw=1, eqw=1, lrn=flat),
      // (prn=inh-2, psn=out-1, cnw=1, eqw=1, lrn=flat)
      ("inh-1", "out-2") -> ConnectionDescription(
        preSynapticNeuronId = "inh-1",
        postSynapticNeuronId = "out-2",
        initialWeight = 1,
        equilibriumWeight = 1,
        learningFunctionName = "flat"
      ),
      ("inh-2", "out-1") -> ConnectionDescription(
        preSynapticNeuronId = "inh-2",
        postSynapticNeuronId = "out-1",
        initialWeight = 1,
        equilibriumWeight = 1,
        learningFunctionName = "flat"
      )
    )
  }

  def learningFunctionDescriptions(): Map[String, LearningFunctionDescription] = {
    Map(
      // (fnc=stdp_alpha, bln=-1, alr=0.04, atc=22 ms)
      "stdp_alpha" -> LearningFunctionDescription(StdpAlphaLearningParams(
        baseline = -1,
        timeConstant = Milliseconds(22),
        learningRate = 0.04
      )),
      // (fnc=stdp_soft, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms)
      "stdp_soft" -> LearningFunctionDescription(StdpSoftLimitLearningParams(
        inhibitionAmplitude = 0.06,
        inhibitionPeriod = Milliseconds(15),
        excitationAmplitude = 0.02,
        excitationPeriod = Milliseconds(10)
      )),
      // (fnc=stdp_hard, ina=0.08, inp=23 ms, exa=0.04, exp=13 ms)
      "stdp_hard" -> LearningFunctionDescription(StdpHardLimitLearningParams(
        inhibitionAmplitude = 0.08,
        inhibitionPeriod = Milliseconds(23),
        excitationAmplitude = 0.04,
        excitationPeriod = Milliseconds(13)
      )),
      // (fnc=flat)
      "flat" -> LearningFunctionDescription(NoLearningParams())
    )
  }

  def wtaNetworkDna(): String = {
      """
        |// line sensor network
        |// For parameters that accept units, if they are not specified, they default to:
        |// • distances to µm
        |// • times to ms
        |// • conductance speeds to m/s
        |// • electric potentials to mV
        |// • frequencies to Hz
        |// • magnetic flux to Wb
        |// notes
        |// • wnm from 1e-3 to 0
        |// • ipl from 0.001 to 0.00 for output layer
        |// • mpn from 0.05 to 0.0
        |(
        |GRP=[
        |    (gid=group1),
        |    (gid=group2, hst=192.168.1.174, prt=2552)
        |
        |],
        |NRN=[
        |    // input layer
        |    (nid=in-1, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        |        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        |        WDF=(fnc=zer),
        |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        |        LOC=(cst=ct, px1=-300 µm, px2=0µm, px3=100 µm)
        |    ),
        |    (nid=in-2, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        |        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        |        WDF=(fnc=zer),
        |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        |        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=100 µm)
        |    ),
        |
        |    // inhibition neuron
        |    (nid=inh-1, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        |        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        |        WDF=(fnc=exp, dhl=10 s),
        |        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        |        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        |        LOC=(cst=ct, px1=-290 µm, px2=0 µm, px3=0 µm)
        |    ),
        |    (nid=inh-2, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        |        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        |        WDF=(fnc=exp, dhl=10 s),
        |        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        |        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        |        LOC=(cst=ct, px1=290 µm, px2=0 µm, px3=0 µm)
        |    ),
        |
        |    // output layer
        |    (nid=out-1, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        |        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        |        WDF=(fnc=zer),
        |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        |        LOC=(cst=ct, px1=-300 µm, px2=0 µm, px3=0 µm)
        |    ),
        |    (nid=out-2, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        |        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        |        WDF=(fnc=zer),
        |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        |        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=0 µm)
        |    )
        |],
        |
        |CON=[
        |    // input to output
        |    (prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_alpha),
        |    //(prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_soft),
        |    //(prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_hard),
        |
        |    // output to inhibition
        |    //(prn=out-1, psn=inh-1, cnw=1, eqw=1, lrn=stdp_hard),
        |    //(prn=out-2, psn=inh-2, cnw=1, eqw=1, lrn=stdp_hard),
        |    (prn=out-1, psn=inh-1, cnw=1, eqw=1, lrn=flat),
        |    (prn=out-2, psn=inh-2, cnw=1, eqw=1, lrn=flat),
        |
        |    // inhib to output
        |    //(prn=inh-1, psn=out-2, cnw=1, eqw=1, lrn=stdp_hard),
        |    //(prn=inh-2, psn=out-1, cnw=1, eqw=1, lrn=stdp_hard)
        |    (prn=inh-1, psn=out-2, cnw=1, eqw=1, lrn=flat),
        |    (prn=inh-2, psn=out-1, cnw=1, eqw=1, lrn=flat),,,,
        |],
        |
        |LRN=[
        |    //(fnc=stdp_soft, ina=0.04, inp=30 ms, exa=0.02, exp=10 ms),
        |    (fnc=stdp_soft, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms),
        |    (fnc=stdp_hard, ina=0.08, inp=23 ms, exa=0.04, exp=13 ms),
        |    //(fnc=stdp_hard, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms),
        |    //(fnc=stdp_alpha, bln=-1, alr=0.02, atc=22 ms),
        |    //(fnc=stdp_alpha, bln=-1, alr=0.02, atc=22 ms),
        |    (fnc=stdp_alpha, bln=-1, alr=0.04, atc=22 ms),
        |    (fnc=flat)
        |]
        |)
      """.stripMargin
  }

  def invalidNetwork_extraCommaInConnections(): String = {
    """
      |(
      |GRP=[
      |    (gid=group1),
      |    (gid=group2, hst=192.168.1.174, prt=2552)
      |],
      |NRN=[
      |    // input layer
      |    (nid=in-1, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
      |        ipb=0 mV, ipl=0 mV, ipd=3600 s,
      |        WDF=(fnc=zer),
      |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
      |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
      |        LOC=(cst=ct, px1=-300 µm, px2=0µm, px3=100 µm)
      |    ),
      |    (nid=in-2, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
      |        ipb=0 mV, ipl=0 mV, ipd=3600 s,
      |        WDF=(fnc=zer),
      |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
      |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
      |        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=100 µm)
      |    ),
      |
      |    // output layer
      |    (nid=out-1, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
      |        ipb=0 mV, ipl=0 nV, ipd=3600 s,
      |        WDF=(fnc=zer),
      |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
      |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
      |        LOC=(cst=ct, px1=-300 µm, px2=0 µm, px3=0 µm)
      |    ),
      |    (nid=out-2, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
      |        ipb=0 mV, ipl=0 nV, ipd=3600 s,
      |        WDF=(fnc=zer),
      |        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
      |        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
      |        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=0 µm)
      |    )
      |],
      |
      |CON=[
      |    // input to output
      |    (prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_alpha, invalid=3),
      |],
      |
      |LRN=[
      |    (fnc=stdp_soft, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms),
      |    (fnc=stdp_hard, ina=0.08, inp=23 ms, exa=0.04, exp=13 ms),
      |    (fnc=stdp_alpha, bln=-1, alr=0.04, atc=22 ms),
      |    (fnc=flat)
      |]
      |)
    """.stripMargin
  }
}

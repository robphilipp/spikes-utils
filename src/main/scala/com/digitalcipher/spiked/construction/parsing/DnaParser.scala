package com.digitalcipher.spiked.construction.parsing

import com.digitalcipher.spiked.construction.description.BistableIntegratorParams.{LIMIT_CYCLE_THRESHOLD, RESTING_STATE_THRESHOLD, TONIC_FIRE_RATE}
import com.digitalcipher.spiked.construction.description.ConnectionDescription.{apply => _, _}
import com.digitalcipher.spiked.construction.description.GroupDescription.{GROUPS, GROUP_ID, HOST, PORT}
import com.digitalcipher.spiked.construction.description.LearningFunctionDescription.{apply => _, _}
import com.digitalcipher.spiked.construction.description.LocationDescription._
import com.digitalcipher.spiked.construction.description.MonostableIntegratorParams.SPIKE_THRESHOLD
import com.digitalcipher.spiked.construction.description.NeuronDescription.{NEURON_GROUP, _}
import com.digitalcipher.spiked.construction.description.SignalReleaseProbabilityDescription.{apply => _, _}
import com.digitalcipher.spiked.construction.description.WeightDecayDescription.{apply => _, _}
import com.digitalcipher.spiked.construction.description.WeightLimitDescription.{apply => _, _}
import com.digitalcipher.spiked.construction.description._
import com.digitalcipher.spiked.construction.parsing.DnaParser._
import com.digitalcipher.spiked.construction.validation.NetworkValidator
import squants.electro._
import squants.motion.{MetersPerSecond, Velocity}
import squants.space.{Angle, Length, Microns, Radians}
import squants.time.{Frequency, Hertz, Milliseconds, Time}

import scala.util.parsing.combinator.RegexParsers

/**
  * The parser for the DNA strand
  * Grammar {{{
  * description ::= "(" keyValue \{ "," keyValue \} ")".
  * list ::= "[" values "]".
  * values ::= value \{ "," value \}.
  * set ::= "{" values "}".
  * idSet ::= id set.
  * keyValue ::= key "=" value.
  * key ::= regex([a-zA-Z]+[0-9]*\w*).
  * range ::= integer ":" integer ":" integer.
  * double ::= regex([-]?[0-9]+\.[0-9]*).
  * id ::= regex([a-zA-Z0-9]+[\-]*[a-zA-Z0-9]).
  * }}}
  *
  * For example {{{
// line sensor network
// For parameters that accept units, if they are not specified, they default to:
// • distances to µm
// • times to ms
// • conductance speeds to m/s
// • electric potentials to mV
// • frequencies to Hz
// • magnetic flux to Wb
// notes
// • wnm from 1e-3 to 0
// • ipl from 0.001 to 0.00 for output layer
// • mpn from 0.05 to 0.0
(
GRP=[
    (gid=group1),
    (gid=group2, hst=192.168.1.174, prt=2552)

],
NRN=[
    // input layer
    (nid=in-1, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=-300 µm, px2=0µm, px3=100 µm)
    ),
    (nid=in-2, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=100 µm)
    ),

    // inhibition neuron
    (nid=inh-1, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=exp, dhl=10 s),
        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        LOC=(cst=ct, px1=-290 µm, px2=0 µm, px3=0 µm)
    ),
    (nid=inh-2, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=exp, dhl=10 s),
        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        LOC=(cst=ct, px1=290 µm, px2=0 µm, px3=0 µm)
    ),

    // output layer
    (nid=out-1, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=-300 µm, px2=0 µm, px3=0 µm)
    ),
    (nid=out-2, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=0 µm)
    )
],

CON=[
    // input to output
    (prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_alpha),
    //(prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_soft),
    //(prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_hard),

    // output to inhibition
    //(prn=out-1, psn=inh-1, cnw=1, eqw=1, lrn=stdp_hard),
    //(prn=out-2, psn=inh-2, cnw=1, eqw=1, lrn=stdp_hard),
    (prn=out-1, psn=inh-1, cnw=1, eqw=1, lrn=flat),
    (prn=out-2, psn=inh-2, cnw=1, eqw=1, lrn=flat),

    // inhib to output
    //(prn=inh-1, psn=out-2, cnw=1, eqw=1, lrn=stdp_hard),
    //(prn=inh-2, psn=out-1, cnw=1, eqw=1, lrn=stdp_hard)
    (prn=inh-1, psn=out-2, cnw=1, eqw=1, lrn=flat),
    (prn=inh-2, psn=out-1, cnw=1, eqw=1, lrn=flat),,,,
],

LRN=[
    //(fnc=stdp_soft, ina=0.04, inp=30 ms, exa=0.02, exp=10 ms),
    (fnc=stdp_soft, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms),
    (fnc=stdp_hard, ina=0.08, inp=23 ms, exa=0.04, exp=13 ms),
    //(fnc=stdp_hard, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms),
    //(fnc=stdp_alpha, bln=-1, alr=0.02, atc=22 ms),
    //(fnc=stdp_alpha, bln=-1, alr=0.02, atc=22 ms),
    (fnc=stdp_alpha, bln=-1, alr=0.04, atc=22 ms),
    (fnc=flat)
]
)
  * }}}
  *
  * Note that units of time are `ms`, units of electric potential are `mV`, ad units of distance are `mm`.
  */
class DnaParser(validateReferences: Boolean) extends RegexParsers {

  /**
    * Parse the DNA sequence into a network description
    *
    * @param dna The DNA sequence to be parsed
    * @return A network description from the parsed DNA sequence
    */
  def parseDna(dna: String): Either[Seq[String], NetworkDescription] = {
    parseAll(description, cleanDna(dna)) match {
      case Success(result, _) =>
        val groupResults = result(GROUPS.name).asInstanceOf[List[Either[String, (String, GroupDescription)]]]
        val neuronResults = result(NEURONS.name).asInstanceOf[List[Either[String, (String, NeuronDescription)]]]
        val connectionResults = result(CONNECTIONS.name).asInstanceOf[List[Either[String, ConnectionDescription]]]
        val learningResults = result(LEARNING_FUNCTIONS.name).asInstanceOf[List[Either[String, (String, LearningFunctionDescription)]]]

        // find any failures parsing the neurons, connections, learning function, or weight-stickiness function
        val groupResultFailure = groupResults.filter(either => either.isLeft).map(either => either.left.get)
        val neuronResultFailure = neuronResults.filter(either => either.isLeft).map(either => either.left.get)
        val connectionResultFailure = connectionResults.filter(either => either.isLeft).map(either => either.left.get)
        val learningResultFailures = learningResults.filter(either => either.isLeft).map(either => either.left.get)

        if (groupResultFailure.nonEmpty ||
          neuronResultFailure.nonEmpty ||
          connectionResultFailure.nonEmpty ||
          learningResultFailures.nonEmpty) {
          return Left(groupResultFailure ::: neuronResultFailure ::: connectionResultFailure ::: learningResultFailures)
        }

        // pull out the successful results
        val groupResultSuccess = groupResults.filter(either => either.isRight).map(either => either.right.get).toMap
        val neuronResultSuccess = neuronResults.filter(either => either.isRight).map(either => either.right.get).toMap
        val connectionResultSuccess = connectionResults.filter(either => either.isRight).map(either => either.right.get)
        val learningResultSuccess = learningResults.filter(either => either.isRight).map(either => either.right.get).toMap

        val networkDescription = new NetworkDescription(
          groups = groupResultSuccess,
          neurons = neuronResultSuccess,
          connections = connectionResultSuccess,
          learningFunctions = learningResultSuccess
        )

        if(validateReferences) {
          NetworkValidator.validateReferences(networkDescription)
        } else {
          Right(networkDescription)
        }

      case Failure(message, next) => Left(List(message.toString, next.toString))

      case _ => Left(List("Parsing neither succeeded nor failed. Strange."))
    }
  }

  /**
    * Cleans the DNA string before handing it to the parser
    * @param dna The DNA string to be cleaned
    * @return The DNA string with trailing commas, comments, and white-space removed
    */
  private def cleanDna(dna: String): String = stripSpaces(stripComments(stripTrailingCommas(dna)))

  /**
    * Removes all white space characters
    * @param dna The raw string
    * @return The DNA stripped of all whitespaces
    */
  private def stripSpaces(dna: String): String = dna.replaceAll("""\s""", "")

  /**
    * Strips comments for the string before the parsing starts
    *
    * @param dna The raw string
    * @return The DNA stripped of comments
    */
  private def stripComments(dna: String): String = dna
      // get rid of the /*...*/ comments
      .replaceAll("""/\*(.*?)[\r\n]*(.*?)\*/""", "")
      // get rid of the //... comments
      .replaceAll("""//[^\r\n]*""", "")


  /**
    * Strips all the new-line characters from the string before parsing
    * @param dna The raw string
    * @return The DNA stripped of new-line characters
    */
  private def stripTrailingCommas(dna: String): String = dna.replaceAll("""[,]+([\n\r]\])+""", "$1")

  /**
    * Enhances the way to specify Volts. With squants, can only specify mV or V. This method allows users to specify
    * nV, µV, mV, and V and converts the units appropriately
    *
    * @param potential The potential as a number of a dimensioned-number
    * @return The electric potential converted from the number of dimensioned number
    */
  private def electricPotential(potential: Any): ElectricPotential = {
    val dimension = """([\d\.eE ]*)([nµm]?V)""".r
    val convertedPotential = potential.toString match {
      case dimension(value, "nV") => s"${value.toDouble * 1e-6}mV"
      case dimension(value, "µV") => s"${value.toDouble * 1e-3}mV"
      case dimension(value, "mV") => s"${value.toDouble}mV"
      case dimension(value, "V") => s"${value.toDouble * 1e3}mV"
      case _ => potential.toString
    }
    ElectricPotential(convertedPotential.toString).getOrElse(Millivolts(potential.asInstanceOf[Double]))
  }

  /**
    * The description is a effectively a map of keys to values
    * {{{description ::= "(" keyValue \{ "," keyValue \} ")".}}}
    *
    * @return a map of the keys to their associated values
    */
  def description: Parser[Map[String, Any]] = "(" ~> repsep(keyValue, ",") <~ ")" ^^ (Map() ++ _)

  /**
    * A list of descriptions that returns a list of map(string->value)
    * {{{list ::= "[" values "]".}}}
    *
    * @return a list of map that represent parameters
    */
  def list: Parser[List[Any]] = "[" ~> values <~ "]" ^^ (_.toList)

  /**
    * A comma-separated list of values
    * {{{values ::= value \{ "," value \}.}}}
    *
    * @return a sequence of any class
    */
  def values: Parser[Seq[Any]] = repsep(value, ",") ^^ (List() ++ _)

  /**
    * A set of values
    * {{{set ::= "{" values "}".}}}
    *
    * @return a set of any class
    */
  def set: Parser[Set[Any]] = "{" ~> values <~ "}" ^^ (_.toSet)

  /**
    * An ID set that expands into a set of IDs. For example, ```hidden-{1:10:1, 20, 27}``` gets converted into the set
    * of IDs ```{hidden-1, hidden-2, ..., hidden-10, hidden-20, hidden-27}```.
    * {{{idSet ::= id set.}}}
    *
    * @return A set of expanded IDs
    */
  def idSet: Parser[Set[Any]] = id ~ set ^^
    (expression => expression._2.map {
      case iterable: Iterable[Any] => iterable.map(element => s"${expression._1}$element").toSet
      case item => s"${expression._1}$item"
    })

  /**
    * The individual (key, value) pairs. Note that although a key is always a string, a value can
    * also be a map
    * {{{keyValue ::= key "=" value.}}}
    *
    * @return A (key, value) tuple
    */
  def keyValue: Parser[(String, Any)] = key ~ "=" ~ value ^^ { case key ~ "=" ~ value => (key, value) }

  //
  // ooo===------< LEARNING FUNCTIONS >------===ooo
  //
  // LRN=[
  //  (fnc=stdp_hard, ina=0.035, inp=3 ms, exa=0.035, exp=3 ms),
  //  (fnc=stdp_soft, ina=0.035, inp=3 ms, exa=0.035, exp=3 ms),
  //  (fnc=stdp_alpha, bln=-1, lnr=0.01, atc=20 ms),
  //  (fnc=flat)
  //  ]
  /**
    * Type of connection-weight learning that depends on the arrival of the pre-synaptic signal relative to the spike
    * time of the post-synaptic neuron.
    * {{{learningType ::= "stdp_soft" | "stdp_hard" | "flat".}}}
    *
    * @return A [[Parser]] holding the learning type name
    */
  def learningType: Parser[String] = STDP_HARD.name | STDP_SOFT.name | STDP_ALPHA.name | NO_LEARNING.name

  /**
    * The learning-function key-value pair
    * {{{learningFunction ::= "fnc" = learningType.}}}
    *
    * @return A [[Parser]] holding the learning function key-value pair
    */
  def learningFunction: Parser[(String, String)] = LEARNING_TYPE.name ~ "=" ~ learningType ^^ {
    case functionName ~ "=" ~ learningType => (functionName, learningType)
  }

  /**
    * The inhibition amplitude for STDP learning
    * {{{inhibitionAmplitude ::= "ina" = number.}}}
    *
    * @return A [[Parser]] holding the inhibition amplitude for STDP learning
    */
  def inhibitionAmplitude: Parser[(String, Double)] = INHIBITION_AMPLITUDE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ amplitude => (key, amplitude)
  }

  /**
    * The inhibition period for STDP learning
    * {{{inhibitionAmplitude ::= "inp" = (dimensionedValue | number).}}}
    *
    * @return A [[Parser]] holding the inhibition amplitude for STDP learning
    */
  def inhibitionPeriod: Parser[(String, Time)] = INHIBITION_PERIOD.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ period => (key, Time(period.toString).getOrElse(Milliseconds(period.asInstanceOf[Double])))
  }

  /**
    * The excitation amplitude for STDP learning
    * {{{excitationAmplitude ::= "exa" = number.}}}
    *
    * @return A [[Parser]] holding the excitation amplitude for STDP learning
    */
  def excitationAmplitude: Parser[(String, Double)] = EXCITATION_AMPLITUDE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ amplitude => (key, amplitude)
  }

  /**
    * The excitation period for STDP learning
    * {{{excitationAmplitude ::= "exp" = (dimensionedValue | number).}}}
    *
    * @return A [[Parser]] holding the excitation amplitude for STDP learning
    */
  def excitationPeriod: Parser[(String, Time)] = EXCITATION_PERIOD.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ period => (key, Time(period.toString).getOrElse(Milliseconds(period.asInstanceOf[Double])))
  }

  /**
    * Parameters describing the learning function
    * {{{learningParam ::= inhibitionAmplitude | inhibitionPeriod | excitationAmplitude | excitationPeriod.}}}
    *
    * @return A [[Parser]] holding a learning parameter
    */
  def learningParam: Parser[(String, Any)] = inhibitionAmplitude | inhibitionPeriod | excitationAmplitude | excitationPeriod

  /**
    * The alpha-function baseline (inhibition amplitude)
    * {{{alphaStdpBaseline ::= "bln" = number.}}}
    *
    * @return A [[Parser]] holding the alpha-function baseline value
    */
  def alphaStdpBaseline: Parser[(String, Double)] = ALPHA_BASELINE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ baseline => (key, baseline)
  }

  /**
    * The alpha-function time-constant
    * {{{alphaTimeConstant ::= "atc" = (dimensionedValue | number).}}}
    *
    * @return A [[Parser]] holding the alpha-function time-constant
    */
  def alphaTimeConstant: Parser[(String, Time)] = ALPHA_TIME_CONSTANT.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ timeConstant => (key, Time(timeConstant.toString).getOrElse(Milliseconds(timeConstant.asInstanceOf[Double])))
  }

  /**
    * The alpha-function learning rate
    * {{{alphaStdpLearningRate ::= "alr" = number.}}}
    *
    * @return A [[Parser]] holding the alpha-function learning rate
    */
  def alphaStdpLearningRate: Parser[(String, Double)] = ALPHA_LEARNING_RATE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ learningRate => (key, learningRate)
  }

  /**
    * Parameters describing the alpha-learning function
    * {{{alphaLearningParam ::= alphaStdpBaseline | alphaStdpLearningRate | alphaTimeConstant.}}}
    *
    * @return A [[Parser]] holding an alpha-learning parameter
    */
  def alphaLearningParam: Parser[(String, Any)] = alphaStdpBaseline | alphaStdpLearningRate | alphaTimeConstant

  /**
    * The hard-limit stdp learning function description
    * {{{stdpHardLearning ::= "(" \{ "fnc" = "stdp_hard", learningParam "," \} ")".}}}
    *
    * @return A [[Parser]] holding a pair containing the learning function name and its associated description
    */
  def stdpHardLearning: Parser[Either[String, (String, LearningFunctionDescription)]] =
    "(" ~ LEARNING_TYPE.name ~ "=" ~ STDP_HARD.name ~ "," ~> repsep(learningParam, ",") <~ ")" ^^
      (params => asLearningFunction(STDP_HARD, params.toMap))

  /**
    * The soft-limit stdp learning function description
    * {{{stdpSoftLearning ::= "(" \{ "fnc" = "stdp_soft", learningParam "," \} ")".}}}
    *
    * @return A [[Parser]] holding a pair containing the learning function name and its associated description
    */
  def stdpSoftLearning: Parser[Either[String, (String, LearningFunctionDescription)]] =
    "(" ~ LEARNING_TYPE.name ~ "=" ~ STDP_SOFT.name ~ "," ~> repsep(learningParam, ",") <~ ")" ^^
      (params => asLearningFunction(STDP_SOFT, params.toMap))

  /**
    * The alpha-function stdp learning function description
    * {{{stdpAlphaLearning ::= "(" \{ "fnc" = "stdp_alpha", alphaLearningParam "," \} ")".}}}
    *
    * @return A [[Parser]] holding a pair containing the learning function name and its associated description
    */
  def stdpAlphaLearning: Parser[Either[String, (String, LearningFunctionDescription)]] =
    "(" ~ LEARNING_TYPE.name ~ "=" ~ STDP_ALPHA.name ~ "," ~> repsep(alphaLearningParam, ",") <~ ")" ^^
      (params => asLearningFunction(STDP_ALPHA, params.toMap))

  /**
    * The function description for no learning
    * {{{noLearning ::= "(" \{ "fnc" = "flat" ")".}}}
    *
    * @return A [[Parser]] holding a pair containing the learning function name and its associated description
    */
  def noLearning: Parser[Either[String, (String, LearningFunctionDescription)]] =
    "(" ~ LEARNING_TYPE.name ~ "=" ~ NO_LEARNING.name ~ ")" ^^
      (_ => asLearningFunction(NO_LEARNING, Map.empty))


  //
  // ooo===------< SYNAPSE TIMING (facilitation and depletion >------===ooo
  //
  // SNT=(fcb=5.0, fcm=0.1, fct=100 ms, dpb=10, dpm=1, dpt=100 ms)
  /**
    * The facilitation base
    * {{{facilitationBase ::= "fcb" = base.}}}
    *
    * @return A [[Parser]] holding the facilitation base
    */
  def facilitationBase: Parser[(String, Double)] = FACILITATION_BASE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ base => (key, base)
  }

  /**
    * The facilitation magnitude
    * {{{facilitationMagnitude ::= "fcm" = magnitude.}}}
    *
    * @return A [[Parser]] holding the facilitation magnitude
    */
  def facilitationMagnitude: Parser[(String, Double)] = FACILITATION_MAGNITUDE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ magnitude => (key, magnitude)
  }

  /**
    * The facilitation time-constant
    * {{{facilitationTimeConstant ::= "fct" = timeConstant.}}}
    *
    * @return A [[Parser]] holding the facilitation time-constant
    */
  def facilitationTimeConstant: Parser[(String, Time)] = FACILITATION_TIME_CONSTANT.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ halfLife => (key, Time(halfLife.toString).getOrElse(Milliseconds(halfLife.asInstanceOf[Double])))
  }

  /**
    * The depletion base
    * {{{depletionBase ::= "dpb" = base.}}}
    *
    * @return A [[Parser]] holding the depletion base
    */
  def depletionBase: Parser[(String, Double)] = DEPLETION_BASE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ base => (key, base)
  }

  /**
    * The depletion magnitude
    * {{{depletionMagnitude ::= "dpm" = magnitude.}}}
    *
    * @return A [[Parser]] holding the depletion magnitude
    */
  def depletionMagnitude: Parser[(String, Double)] = DEPLETION_MAGNITUDE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ magnitude => (key, magnitude)
  }

  /**
    * The depletion time-constant
    * {{{depletionTimeConstant ::= "dpt" = timeConstant.}}}
    *
    * @return A [[Parser]] holding the depletion time-constant
    */
  def depletionTimeConstant: Parser[(String, Time)] = DEPLETION_TIME_CONSTANT.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ halfLife => (key, Time(halfLife.toString).getOrElse(Milliseconds(halfLife.asInstanceOf[Double])))
  }

  /**
    * Valid parameters describing the synapse timing function
    * {{{synapseTimingParam ::= facilitationBase | facilitationMagnitude | facilitationTimeConstant |
    *     depletionBase | depletionMagnitude | depletionTimeConstant.}}}
    *
    * @return valid parameters describing the synapse timing function
    */
  def synapseTimingParam: Parser[(String, Any)] =
    facilitationBase | facilitationMagnitude | facilitationTimeConstant |
      depletionBase | depletionMagnitude | depletionTimeConstant

  /**
    * The synapse timing function description
    * {{{synapseTime ::= "(" synapseTimingParam \{ "," synapseTimingParam \} ")"}}}
    *
    * @return a [[Parser]] holding the synapse timing functoin description
    */
  def synapseTiming: Parser[SignalReleaseProbabilityDescription] = "(" ~> repsep(synapseTimingParam, ",") <~ ")" ^^
    (params => asSynapseTiming(params.toMap))

  //
  // ooo===------< WEIGHT LIMITER >------===ooo
  //
  // WLF=(fnc=bnd, lwb=0.0, upb=5.0)
  /**
    * The weight limiter type
    * {{{weightLimitType ::= "bnd" | "unb". }}}
    *
    * @return A [[Parser]] holding the weight limiter type function name
    */
  def weightLimitType: Parser[String] = BOUNDED.name | UNBOUNDED.name

  /**
    * The weight limiter function
    * {{{weightLimitFunction ::= "fnc" = weightLimitType.}}}
    *
    * @return A [[Parser]] holding a pair describing the weight limiter function name ("fnc", weightLimitTpe)
    */
  def weightLimitFunction: Parser[(String, String)] = LIMITER_TYPE.name ~ "=" ~ weightLimitType ^^ {
    case functionName ~ "=" ~ limiterType => (functionName, limiterType)
  }

  /**
    * The connection weight's lower bound (i.e. the smallest value it can take)
    * {{{weightLimitLowerBound ::= "lwb" = number.}}}
    *
    * @return A [[Parser]] holding the weight's lower bound
    */
  def weightLimitLowerBound: Parser[(String, Double)] = LOWER_BOUND.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ bound => (key, bound)
  }

  /**
    * The connection weight's upper bound (i.e. the largest value it can take)
    * {{{weightLimitUpperBound ::= "upb" = number.}}}
    *
    * @return A [[Parser]] holding the weight's upper bound
    */
  def weightLimitUpperBound: Parser[(String, Double)] = UPPER_BOUND.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ bound => (key, bound)
  }

  /**
    * The valid parameters describing weight limit functions
    * {{{weightLimitParam ::= weightLimitFunction | weightLimitLowerBound | weightLimitUpperBound.}}}
    *
    * @return A [[Parser]] holding a list of weight limiter function parameters
    */
  def weightLimitParam: Parser[(String, Any)] = weightLimitFunction | weightLimitLowerBound | weightLimitUpperBound

  /**
    * The weight limiter function description
    * {{{weightLimit ::= "(" weightLimitParam \{ "," weightLimitParam \} ")"}}}
    *
    * @return A [[Parser]] holding the weight limiter function description
    */
  def weightLimit: Parser[WeightLimitDescription] = "(" ~> repsep(weightLimitParam, ",") <~ ")" ^^
    (params => asWeightLimiter(params.toMap))

  //
  // ooo===------< WEIGHT DECAY >------===ooo
  //
  // WDF=(fnc=exp, dhl=1000) | WDF=(fnc=zer)
  /**
    * Valid weight decay function types
    * {{{weightDecayType ::= "exp" | "zer".}}}
    *
    * @return A [[Parser]] holding the weight decay function type
    */
  def weightDecayType: Parser[String] = EXPONENTIAL.name | ZERO.name

  /**
    * The weight-decay function type key-value pair
    * {{{weightDecayFunction ::= "fnc" = weightDecayType.}}}
    *
    * @return A [[Parser]] holding the weight decay function type pair (i.e. ("fnc", weightDecayType))
    */
  def weightDecayFunction: Parser[(String, String)] = DECAY_TYPE.name ~ "=" ~ weightDecayType ^^ {
    case functionName ~ "=" ~ decayType => (functionName, decayType)
  }

  /**
    * The weight decay half-life needed for the exponential weight decay
    * {{{weightDecayHalfLife ::= "dhl" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the weight decay function half-life for exponential decay
    */
  def weightDecayHalfLife: Parser[(String, Time)] = DECAY_HALF_LIFE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ halfLife => (key, Time(halfLife.toString).getOrElse(Milliseconds(halfLife.asInstanceOf[Double])))
  }

  /**
    * Valid weight decay parameter
    * {{{weightDecayParam ::= weightDecayFunction | weightDecayHalfLife. }}}
    *
    * @return A [[Parser]] holding a valid weight decay parameter
    */
  def weightDecayParam: Parser[(String, Any)] = weightDecayFunction | weightDecayHalfLife

  /**
    * The weight-decay function description
    * {{{weightDecay ::= "(" weightDecayParam \{ "," weightDecayParam \} ")".}}}
    *
    * @return A [[Parser]] holding the weight decay function description
    */
  def weightDecay: Parser[WeightDecayDescription] = "(" ~> repsep(weightDecayParam, ",") <~ ")" ^^
    (params => asWeightDecay(params.toMap))

  //
  // ooo===------< CONNECTIONS >------===ooo
  //
  //(prn=input-1{1:7:1}, psn=hidden-1, cnw=1.5, eqw=1.5, lrn=stdp, cst=sig)

  /**
    * Pre-synaptic neuron identified by an ID, which could be an individual number or a set of numbers
    * {{{preSynapticNeuron ::= "prn" = idSet | id.}}}
    *
    * @return a [[Parser]] holding the pair, `("prn", set(neuron ID))`
    */
  def preSynapticNeuron: Parser[(String, Set[String])] = PRE_SYNAPTIC_NEURON.name ~ "=" ~ (idSet | id) ^^ {
    case key ~ "=" ~ neurons => (key, asSet(neurons, Set()))
  }

  /**
    * Post-synaptic neuron identified by an ID, which could be an individual number or a set of numbers
    * {{{preSynapticNeuron ::= "psn" = idSet | id.}}}
    *
    * @return a [[Parser]] holding the pair, `("psn", set(neuron ID))`
    */
  def postSynapticNeuron: Parser[(String, Set[String])] = POST_SYNAPTIC_NEURON.name ~ "=" ~ (idSet | id) ^^ {
    case key ~ "=" ~ neurons => (key, asSet(neurons, Set()))
  }

  /**
    * The initial connection weight when the neuron is constructed.
    * {{{initialConnectionWeight ::= "cnw" = number.}}}
    *
    * @return A [[Parser]] holding the pair, `("cnw", number)`
    */
  def initialConnectionWeight: Parser[(String, Double)] = INITIAL_CONNECTION_WEIGHT.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ weight => (key, weight)
  }

  /**
    * The initial connection weight, drawn from a random number
    * {{{randomConnectionWeight ::= "cnw" = r random}}}
    *
    * @return A [[Parser]] holding the pair, `("cnw", number)` where the number is random
    */
  def randomConnectionWeight: Parser[(String, Double)] = INITIAL_CONNECTION_WEIGHT.name ~ "=" ~ random ^^ {
    case key ~ "=" ~ weight => (key, weight)
  }

  /**
    * The equilibrium connection weight when the neuron is constructed, which is the value to which the weight decays.
    * {{{equilibriumConnectionWeight ::= "eqw" = number.}}}
    *
    * @return A [[Parser]] holding the pair, `("eqw", number)`
    */
  def equilibriumConnectionWeight: Parser[(String, Double)] = EQUILIBRIUM_CONNECTION_WEIGHT.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ weight => (key, weight)
  }

  /**
    * The equilibrium connection weight, drawn from a random number
    * {{{equilibriumRandomConnectionWeight ::= "eqw" = r random}}}
    *
    * @return A [[Parser]] holding the pair, `("eqw", number)` where the number is random
    */
  def equilibriumRandomConnectionWeight: Parser[(String, Double)] = EQUILIBRIUM_CONNECTION_WEIGHT.name ~ "=" ~ random ^^ {
    case key ~ "=" ~ weight => (key, weight)
  }

  /**
    * The learning function name (which must match a specified learning function)
    * {{{connectionLearning ::= "lrn" = id.}}}
    *
    * @return A [[Parser]] holding the pair, `("lrn", name)`
    */
  def connectionLearning: Parser[(String, String)] = LEARNING_FUNCTION_NAME.name ~ "=" ~ id ^^ {
    case key ~ "=" ~ learningFunctionName => (key, learningFunctionName)
  }

  /**
    * Required, and allowed, connection parameters
    * {{{connectionParameter ::=
    *   preSynapticNeuron |
    *   postSynapticNeuron |
    *   initialConnectionWeight |
    *   equilibriumConnectionWeight |
    *   equilibriumConnectionWeight |
    *   equilibriumRandomConnectionWeight |
    *   connectionLearning |
    *   connectionStickiness
    * }}}
    *
    * @return A [[Parser]] holding a pair where the 1^st^ parameter is the connection-paramater name, and the 2^nd^
    *         parameter is the value of that connection parameter.
    */
  def connectionParameter: Parser[(String, Any)] = preSynapticNeuron | postSynapticNeuron |
    initialConnectionWeight | randomConnectionWeight |
    equilibriumConnectionWeight | equilibriumRandomConnectionWeight |
    connectionLearning

  /**
    * A neuron connection list. Note that because the pre- and post-synaptic neuron IDs can have ranges in them, a single
    * line that matches here could translate into a list of more than one connections. And becuase of that, this returns
    * a list of connection description instances.
    * {{{connection ::= "(" connectionParameter \{ "," connectionParameter \} ")".}}}
    *
    * @return A [[Parser]] holding a list of connection description instances
    */
  def connection: Parser[List[Either[String, ConnectionDescription]]] = "(" ~> repsep(connectionParameter, ",") <~ ")" ^^
    (params => asConnection(params.toMap))

  /**
    * A list of connections. Unlike the [[connection]] this is a flattened list of the output from all the lines that
    * match that [[connection]]. This is the final connection list.
    * {{{connections ::= "[" connection \{ "," connection \} "]".}}}
    *
    * @return A [[Parser]] holding the list of connection description instance collected from the [[connection]] method
    */
  def connections: Parser[List[Either[String, ConnectionDescription]]] = "[" ~> repsep(connection, ",") <~ "]" ^^
    (lists => lists.flatten)

  //
  // ooo===------< NEURON GROUPS >------===ooo
  // GRP=[
  //  (gid=id1, hst=192.168.1.174, prt=2552),
  //  (gid=id2, hst=192.168.1.175, prt=2552)
  // ]
  /**
    * The group ID
    * {{{groupId ::= "gid" = id.}}}
    *
    * @return A [[Parser]] holding the group ID
    */
  def groupId: Parser[(String, String)] = GROUP_ID.name ~ "=" ~ id ^^ {
    case key ~ "=" ~ groupId => (key, groupId)
  }

  /**
    * The host (IP or hostname) on which the neuron group executes
    * {{{host ::= "hst" = host.}}}
    *
    * @return A [[Parser]] holding the host key-value pair
    */
  def groupHost: Parser[(String, String)] = HOST.name ~ "=" ~ host ^^ {
    case key ~ "=" ~ host => (key, host)
  }

  /**
    * The port on which akka listens of the specified host
    * {{{port ::= "prt" = port.}}}
    *
    * @return A [[Parser]] holding the port key-value pair
    */
  def groupPort: Parser[(String, Int)] = PORT.name ~ "=" ~ integer ^^ {
    case key ~ "=" ~ port => (key, port)
  }

  /**
    * A valid group parameter
    * {{{groupParam ::= groupId | groupHost | groupPort.}}}
    *
    * @return A [[Parser]] holding the group parameter as a key-value pair
    */
  def groupParam: Parser[(String, Any)] = groupId | groupHost | groupPort

  /**
    * The group description
    * {{{group ::= "(" groupParam \{ "," groupParam \} ")".}}}
    *
    * @return A [[Parser]] holding either the (groupId, groupDescription) pair, or a failure message
    */
  def group: Parser[Either[String, (String, GroupDescription)]] = "(" ~> repsep(groupParam, ",") <~ ")" ^^
    (params => asGroup(params.toMap))

  //
  // ooo===------< NEURON >------===ooo
  // (nid=hidden-1, nty=mi, mst=5 mV, inh=f, rfp=20 ms, mnp=0 mV, mpd=2.5 s, spp=3 mV, csp=1 m/s, WDF=(fnc=exp, dhl=1000), WLF=(fnc=bnd, lwb=0.0, upb=5.0), LOC=(cst=ct, px1=-30 µm, px2=0 µm, px3=50 µm))
  // (nid=inhib-7, nty=bi, lct=5 mV, rst=4.5 mV, tfr=100 Hz, inh=t, rfp=20 ms, mnp=0 mV, mpd=25 ms, spp=0.5 mV, csp=1 m/s, WDF=(fnc=exp, dhl=100 s), WLF=(fnc=bnd, lwb=0.0, upb=5.0), LOC=(cst=ct, px1=30 µm, px2=0 µm, px3=5 µm)),

  /**
    * The neuron ID
    * {{{neuronId ::= "nid" = id}}}
    *
    * @return A [[Parser]] holding the neuron ID
    */
  def neuronId: Parser[(String, String)] = NEURON_ID.name ~ "=" ~ id ^^ {
    case key ~ "=" ~ neuronId => (key, neuronId)
  }

  /**
    * The ID of the group to which the neuron belongs
    * {{{neuronGroup ::= "grp" = id}}}
    *
    * @return A [[Parser]] holding the neuron ID
    */
  def neuronGroup: Parser[(String, String)] = NEURON_GROUP.name ~ "=" ~ id ^^ {
    case key ~ "=" ~ groupId => (key, groupId)
  }

  /**
    * Valid neuron types
    * {{{neuronTypeParam ::= "mi" | "bi".}}}
    *
    * @return A [[Parser]] holding the neuron type
    */
  def neuronTypeParam: Parser[String] = MONOSTABLE_INTEGRATOR.name | BISTABLE_INTEGRATOR.name

  /**
    * The neuron type key-value pair
    * {{{neuronType ::= "nty" = neuronTypeParam.}}}
    *
    * @return A [[Parser]] holding the neuron type key-value pair
    */
  def neuronType: Parser[(String, String)] = NEURON_TYPE.name ~ "=" ~ neuronTypeParam ^^ {
    case key ~ "=" ~ neuronType => (key, neuronType)
  }

  /**
    * The flag specifying whether or not this neuron is an inhibition neuron
    * {{{inhibitionNeuron ::= "inh" = boolean.}}}
    *
    * @return A [[Parser]] holding the inhibition-neuron flag key-value pair
    */
  def inhibitionNeuron: Parser[(String, Boolean)] = INHIBITION.name ~ "=" ~ boolean ^^ {
    case key ~ "=" ~ inhibitor => (key, inhibitor)
  }

  /**
    * The neuron's refractory period
    * {{{refractoryPeriod ::= "rfp" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron refractory period
    */
  def refractoryPeriod: Parser[(String, Time)] = REFRACTORY_PERIOD.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ period => (key, Time(period.toString).getOrElse(Milliseconds(period.asInstanceOf[Double])))
  }

  /**
    * The neuron's base refractoriness
    * {{{baseRefractoriness ::= "rfb" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron refractory period
    */
  def baseRefractoriness: Parser[(String, MagneticFlux)] = BASE_REFRACTORINESS.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ flux =>
      // squants doesn't parse the nWb, µWb, or mWb, so to support this notation in the DNA files,
      // need to take care of that here.
      val fluxDim = """([\d\.eE ]*)([nµm]?)Wb""".r
      val convertedFlux = flux.toString match {
        case fluxDim(value, "n") => s"${value.toDouble * 1e-9}Wb"
        case fluxDim(value, "µ") => s"${value.toDouble * 1e-6}Wb"
        case fluxDim(value, "m") => s"${value.toDouble * 1e-3}Wb"
        case _ => flux.toString
      }
      (key, MagneticFlux(convertedFlux).getOrElse(Webers(flux.asInstanceOf[Double])))
  }

  /**
    * The neuron's minimum allowed value for the membrane potential
    * {{{minimumMembranePotential ::= "mnp" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron's minimum allowed membrane potential
    */
  def minimumMembranePotential: Parser[(String, ElectricPotential)] = MIN_MEMBRANE_POTENTIAL.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ potential => (key, electricPotential(potential))
  }

  /**
    * The neuron's membrane potential decay
    * {{{membranePotentialDecay ::= "mpd" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron's membrane potential decay
    */
  def membranePotentialDecay: Parser[(String, Time)] = MEMBRANE_POTENTIAL_DECAY.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ halfLife => (key, Time(halfLife.toString).getOrElse(Milliseconds(halfLife.asInstanceOf[Double])))
  }

  /**
    * The neuron's membrane potential rise
    * {{{membranePotentialRise ::= "mpr" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron's membrane potential decay
    */
  def membranePotentialRise: Parser[(String, Time)] = MEMBRANE_POTENTIAL_RISE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ halfLife => (key, Time(halfLife.toString).getOrElse(Milliseconds(halfLife.asInstanceOf[Double])))
  }

  /**
    * The magnitude of the neuron's membrane potential noise
    * {{{membranePotentialNoise ::= "mpn" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the magnitude of the neuron's membrane potential noise
    */
  def membranePotentialNoise: Parser[(String, ElectricPotential)] = MEMBRANE_POTENTIAL_NOISE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ noise => (key, electricPotential(noise))
  }

  /**
    * The neuron's spike potential
    * {{{membraneSpikePotential ::= "spp" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron's spike potential
    */
  def membraneSpikePotential: Parser[(String, ElectricPotential)] = SPIKE_POTENTIAL.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ potential => (key, electricPotential(potential))
  }

  /**
    * The neuron's signal conductance speed
    * {{{signalConductanceSpeed ::= "csp" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron's signal conductance speed
    */
  def signalConductanceSpeed: Parser[(String, Velocity)] = CONDUCTANCE_SPEED.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ speed => (key, Velocity(speed.toString).getOrElse(MetersPerSecond(speed.asInstanceOf[Double])))
  }

  /**
    * The connection weight-noise magnitude for the Wiener process describing the noise
    * {{{weightNoiseMagnitude ::= "wnm" = number.}}}
    *
    * @return A [[Parser]] holding the weight noise magnitude
    */
  def weightNoiseMagnitude: Parser[(String, Double)] = WEIGHT_NOISE.name ~ "=" ~ number ^^ {
    case key ~ "=" ~ magnitude => (key, magnitude)
  }

  /**
    * The intrinsic plasticity base value
    * {{{intrinsicPlasticityBase ::= "ipb" = dimensionedNumber | number.}}}
    *
    * @return A [[Parser]] holding the intrinsic plasticity base
    */
  def intrinsicPlasticityBase: Parser[(String, ElectricPotential)] = INTRINSIC_PLASTICITY_BASE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ base => (key, electricPotential(base))
  }

  /**
    * The intrinsic plasticity learning amount per unit time
    * {{{intrinsicPlasticityLearning ::= "ipl" = dimensionedNumber | number.}}}
    *
    * @return A [[Parser]] holding the intrinsic plasticity learning amount per unit time
    */
  def intrinsicPlasticityLearning: Parser[(String, ElectricPotential)] = INTRINSIC_PLASTICITY_LEARNING_RATE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ learning => (key, electricPotential(learning))
  }

  /**
    * The intrinsic plasticity decay half-life
    * {{{intrinsicPlasticityDecayHalfLife ::= diminsionedNumber | number.}}}
    *
    * @return A [[Parser]] holding the intrinsic plasticity decay half-life
    */
  def intrinsicPlasticityDecayHalfLife: Parser[(String, Time)] = INTRINSIC_PLASTICITY_DECAY_HALF_LIFE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ halfLife => (key, Time(halfLife.toString).getOrElse(Milliseconds(halfLife.asInstanceOf[Double])))
  }

  /**
    * The neuron's weight decay function
    * {{{neuronWeightDecay ::= "WDF" = weightDecay.}}}
    *
    * @return A [[Parser]] holding the neuron's weight-decay description
    */
  def neuronWeightDecay: Parser[(String, WeightDecayDescription)] = WEIGHT_DECAY_FUNCTION.name ~ "=" ~ weightDecay ^^ {
    case key ~ "=" ~ weightDecay => (key, weightDecay)
  }

  /**
    * The neuron's synapse-timing function
    * {{{synapseTiming ::= "SNT" = synapseTiming}}}
    *
    * @return A [[Parser]] holding the neuron's signal-release probability description
    */
  def synapseTimingFunction: Parser[(String, SignalReleaseProbabilityDescription)] = SIGNAL_RELEASE_PROBABILITY.name ~ "=" ~ synapseTiming ^^ {
    case key ~ "=" ~ synapseTiming => (key, synapseTiming)
  }

  /**
    * The neuron's weight decay function
    * {{{neuronWeightDecay ::= "WLF" = neuronWeightLimiter.}}}
    *
    * @return A [[Parser]] holding the neuron's weight-limit description
    */
  def neuronWeightLimiter: Parser[(String, WeightLimitDescription)] = WEIGHT_LIMITER_FUNCTION.name ~ "=" ~ weightLimit ^^ {
    case key ~ "=" ~ weightLimit => (key, weightLimit)
  }

  /**
    * The neuron's locaton description
    * {{{neuronLocation ::= "LOC" = location.}}}
    *
    * @return A [[Parser]] holding the neuron's location description
    */
  def neuronLocation: Parser[(String, LocationDescription)] = LOCATION.name ~ "=" ~ location ^^ {
    case key ~ "=" ~ location => (key, location)
  }

  // monostable integrator
  /**
    * The monostable integrator neuron's spike threshold
    * {{{membraneSpikeThreshold ::= "mst" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the neuron's spike threshold
    */
  def membraneSpikeThreshold: Parser[(String, ElectricPotential)] = SPIKE_THRESHOLD.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ threshold => (key, electricPotential(threshold))
  }

  /**
    * Valid monostable integrator parameter
    * {{{monostableIntegratorParam ::= neuronId |
    *   neuronType | membraneSpikeThreshold |
    *   inhibitionNeuron |
    *   refractoryPeriod | baseRefractoriness |
    *   minimumMembranePotential | membranePotentialDecay | membranePotentialRise | membranePotentialNoise |
    *   membraneSpikePotential | weightNoiseMagnitude | signalConductanceSpeed |
    *   intrinsicPlasticityBase | intrinsicPlasticityLearning | intrinsicPlasticityDecayHalfLife |
    *   neuronWeightDecay | neuronWeightLimiter |
    *   neuronLocation
    * }}}
    *
    * @return A [[Parser]] holding the monostable integrator neuron parameter
    */
  def monostableIntegratorParam: Parser[(String, Any)] = neuronId | neuronGroup | neuronType | membraneSpikeThreshold |
    inhibitionNeuron | refractoryPeriod | baseRefractoriness |
    minimumMembranePotential | membranePotentialDecay | membranePotentialRise | membranePotentialNoise |
    membraneSpikePotential | weightNoiseMagnitude | signalConductanceSpeed |
    intrinsicPlasticityBase | intrinsicPlasticityLearning | intrinsicPlasticityDecayHalfLife |
    neuronWeightDecay | neuronWeightLimiter | synapseTimingFunction | neuronLocation

  // bistable integrator
  /**
    * The bistable integrator neuron's threshold to entering the tonic firing limit-cycle
    * {{{limitCycleThreshold ::= "lct" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the bistable integrator neuron's limit-cycle threshold
    */
  def limitCycleThreshold: Parser[(String, ElectricPotential)] = LIMIT_CYCLE_THRESHOLD.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ threshold => (key, electricPotential(threshold))
  }

  /**
    * The bistable integrator neuron's threshold to leaving the tonic firing limit-cycle and entering the resting state
    * {{{limitCycleThreshold ::= "lct" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the bistable integrator neuron's resting-state threshold
    */
  def restingStateThreshold: Parser[(String, ElectricPotential)] = RESTING_STATE_THRESHOLD.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ threshold => (key, electricPotential(threshold))
  }

  /**
    * The bistable integrator neuron's tonic firing rate when it is in the limit-cycle
    * {{{limitCycleThreshold ::= "tfr" = dimensionedValue | number.}}}
    *
    * @return A [[Parser]] holding the bistable integrator neuron's tonic firing rate when it is in the limit-cycle
    */
  def tonicFireRate: Parser[(String, Frequency)] = TONIC_FIRE_RATE.name ~ "=" ~ (dimensionedNumber | number) ^^ {
    case key ~ "=" ~ rate => (key, Frequency(rate.toString).getOrElse(Hertz(rate.asInstanceOf[Double])))
  }

  /**
    * Valid bistable integrator neuron parameter
    * {{{bistableIntegratorParam ::= neuronId |
    *   neuronType | limitCycleThreshold | restingStateThreshold | tonicFireRate |
    *   inhibitionNeuron |
    *   refractoryPeriod | baseRefractoriness |
    *   minimumMembranePotential | membranePotentialDecay | membranePotentialRise | membranePotentialNoise |
    *   membraneSpikePotential | weightNoiseMagnitude | signalConductanceSpeed |
    *   intrinsicPlasticityBase | intrinsicPlasticityLearning | intrinsicPlasticityDecayHalfLife |
    *   neuronWeightDecay | neuronWeightLimiter |
    *   neuronLocation
    * }}}
    *
    * @return A [[Parser]] holding the bistable integrator neuron parameter
    */
  def bistableIntegratorParam: Parser[(String, Any)] = neuronId | neuronGroup | neuronType | limitCycleThreshold | restingStateThreshold | tonicFireRate |
    inhibitionNeuron | refractoryPeriod | baseRefractoriness |
    minimumMembranePotential | membranePotentialDecay | membranePotentialRise | membranePotentialNoise |
    membraneSpikePotential | weightNoiseMagnitude | signalConductanceSpeed |
    intrinsicPlasticityBase | intrinsicPlasticityLearning | intrinsicPlasticityDecayHalfLife |
    neuronWeightDecay | neuronWeightLimiter | synapseTimingFunction | neuronLocation

  /**
    * The a monostable integrator neuron description
    * {{{neuron ::= "(" monostableIntegratorParam \{ "," monostableIntegratorParam \}. ")" }}}
    *
    * @return A [[Parser]] holding the neuron description or a string with the failure message
    */
  def monostableIntegrator: Parser[Either[String, (String, NeuronDescription)]] = "(" ~> repsep(monostableIntegratorParam, ",") <~ ")" ^^
    (params => asNeuron(params.toMap))

  /**
    * The a bistable integrator neuron description
    * {{{neuron ::= "(" bistableIntegratorParam \{ "," bistableIntegratorParam \}. ")" }}}
    *
    * @return A [[Parser]] holding the neuron description or a string with the failure message
    */
  def bistableIntegrator: Parser[Either[String, (String, NeuronDescription)]] = "(" ~> repsep(bistableIntegratorParam, ",") <~ ")" ^^
    (params => asNeuron(params.toMap))

  //
  // ooo===------< LOCATION >------===ooo
  //
  /**
    * Allowable coordinate system names (i.e. cartesian, cylindrical, spherical)
    * {{{coordinateName ::= "ct" | "cl" | "sp".}}}
    *
    * @return A [[Parser]] holding the coordinate-system name
    */
  def coordinateName: Parser[String] = CARTESIAN.name | CYLINDRICAL.name | SPHERICAL.name

  /**
    * A coordinate system key-value pair
    * {{{coordinateType ::= "cst" = name.}}}
    *
    * @return A [[Parser]] holding the name of the coordinate system type (i.e. cartesian, cylindrical, spherical)
    */
  def coordinateType: Parser[String] = COORDINATE_TYPE.name ~ "=" ~ coordinateName ^^ {
    case _ ~ "=" ~ coordinateType => coordinateType
  }

  /**
    * A projection's dimension (i.e. a dimension in the coordinate). For example, if `px1` is specified, then the ''dimension'' of
    * the coordinate is 1. As a further example, given a cartesian coordinate `(x, y, z)`, `px2` would refer to `y` and
    * would have a ''dimension'' of 2.
    * {{{project ::= "px1" | "px2" | "px3".}}}
    *
    * @return A [[Parser]] holding the coordinate ''dimension''.
    */
  def projection: Parser[Int] = (PROJECTION_1.name | PROJECTION_2.name | PROJECTION_3.name) ^^
    (projection => projection.substring(projection.length - 1).toInt)

  /**
    * A coordinate can be with or without dimensions
    * {{{coordinateValue ::= dimensionedValue | integer | double}}}
    *
    * @return A [[Parser]] holding the coordinate value as a string
    */
  def coordinateValue: Parser[String] = dimensionedNumber | double ^^ (_.toString) | integer ^^ (_.toString)

  /**
    * A pair holding the projection's dimension and the associated value. Note that the value is a ''dimensioned value'',
    * and therefore, is represented as a string. For example, the value may be `20 µm`.
    * {{{coordinate ::= projection = dimensionedValue.}}}
    *
    * @return A [[Parser]] holding the coordinate
    */
  def coordinate: Parser[(Int, String)] = projection ~ "=" ~ coordinateValue ^^ {
    case projection ~ "=" ~ value => (projection, value)
  }

  /**
    * A location must have a coordinate-system type (i.e. cartesian, cylindrical, spherical) and then three coordinates
    * (i.e. projection 1, 2, and 3)
    * {{{location ::= ( coordinateType, coordinate, coordinate, coordinate ).}}}
    *
    * @return A [[Parser]] holding a location description object representing the coordinate type
    */
  def location: Parser[LocationDescription] = "(" ~> coordinateType ~ "," ~ coordinate ~ "," ~ coordinate ~ "," ~ coordinate <~ ")" ^^ {
    //noinspection ScalaUnnecessaryParentheses
    case ct ~ "," ~ ((d1, c1)) ~ "," ~ ((d2, c2)) ~ "," ~ ((d3, c3)) => asLocation(ct, (d1, c1), (d2, c2), (d3, c3))
  }

  //
  // ooo===------< KEY-VALUE >------===ooo
  //
  /**
    * A key must start with lower or upper case alpha characters and then can have digits and word characters
    * {{{key ::=
    *   "CST" | "fnc" | "dhl" |
    *   "LRN" | "fnc" | "ina" | "inp" | "exa" | "exp" |
    *   "WLF" | "fnc" | "lwb" | "upb" |
    *   "WDF" | "fnc" | "dhl" |
    *   "SNT" | "fcb" | "fcm" | "fct" | "dpb" | "dpm" | "dpt" |
    *   "CON" | "prn" | "psn" | "cnw" | "eqw" | "lrn" | "cst" |
    *   "GRP" | "gid" | "hst" | "prt" |
    *   "NRN" | "nid" | "grp" | "nty" | "inh" | "rfp" | "mnp" | "mpd" | "mst" | "lct" | "rst" | "tfr" | "wnm" | "ipb" | "ipl" | "ipd" |
    *   "LOC" | "cst" | "px1" | "px2" | "px3"
    * }}}
    *
    * @return A [[Parser]] holding the key
    */
  def key: Parser[String] =
    LEARNING_FUNCTIONS.name |
      LEARNING_TYPE.name | INHIBITION_AMPLITUDE.name | INHIBITION_PERIOD.name | EXCITATION_AMPLITUDE.name | EXCITATION_PERIOD.name |
      WEIGHT_LIMITER_FUNCTION.name |
      LIMITER_TYPE.name | LOWER_BOUND.name | UPPER_BOUND.name |
      WEIGHT_DECAY_FUNCTION.name |
      DECAY_TYPE.name | DECAY_HALF_LIFE.name |
      SIGNAL_RELEASE_PROBABILITY.name |
      FACILITATION_BASE.name | FACILITATION_MAGNITUDE.name | FACILITATION_TIME_CONSTANT.name |
      DEPLETION_BASE.name | DEPLETION_MAGNITUDE.name | DEPLETION_TIME_CONSTANT.name |
      CONNECTIONS.name |
      PRE_SYNAPTIC_NEURON.name | POST_SYNAPTIC_NEURON.name | INITIAL_CONNECTION_WEIGHT.name | EQUILIBRIUM_CONNECTION_WEIGHT.name | LEARNING_FUNCTION_NAME.name |
      GROUPS.name |
      GROUP_ID.name | HOST.name | PORT.name |
      NEURONS.name |
      NEURON_ID.name | NEURON_GROUP.name | NEURON_TYPE.name | INHIBITION.name | REFRACTORY_PERIOD.name | MIN_MEMBRANE_POTENTIAL.name | MEMBRANE_POTENTIAL_DECAY.name |
      SPIKE_POTENTIAL.name | CONDUCTANCE_SPEED.name | WEIGHT_DECAY_FUNCTION.name | WEIGHT_LIMITER_FUNCTION.name | LOCATION.name |
      SPIKE_THRESHOLD.name |
      LIMIT_CYCLE_THRESHOLD.name | RESTING_STATE_THRESHOLD.name | TONIC_FIRE_RATE.name |
      WEIGHT_NOISE.name |
      INTRINSIC_PLASTICITY_BASE.name | INTRINSIC_PLASTICITY_LEARNING_RATE.name | INTRINSIC_PLASTICITY_DECAY_HALF_LIFE.name |
      LOCATION.name |
      COORDINATE_TYPE.name | PROJECTION_1.name | PROJECTION_2.name | PROJECTION_3.name

  /**
    * The value can be a description, or an integer or floating-point number, and a string
    * {{{value ::= location | description | list | range | set | idSet | double | integer | id}}}
    *
    * @return A string or description
    */
  def value: Parser[Any] =
    monostableIntegrator | bistableIntegrator |
      //      learning |
      stdpHardLearning | stdpSoftLearning | stdpAlphaLearning | noLearning |
      weightLimit |
      weightDecay |
      synapseTiming |
      connections |
      connection |
      group |
      location |
      description |
      list |
      range |
      set |
      host |
      idSet |
      dimensionedNumber |
      double |
      integer |
      id |
      boolean

  //
  // ooo===------< DIMENSIONS >------===ooo
  //
  /**
    * Dimensions must be 1 to 4 characters long, and can be unicode characters
    * {{{dimension ::= regex([a-zA-Z\p{L}\p{M}]{1,4})}}}
    *
    * @return A string
    */
  def dimension: Parser[String] =
    """[a-zA-Z\p{L}\p{M}/]{1,4}""".r

  /**
    * A floating point value with an attached dimension
    * {{{dimensionedDouble ::= double dimension}}}
    *
    * @return A string representing the double with the attached dimension
    */
  def dimensionedDouble: Parser[String] = double ~ dimension ^^ (tuple => s"${tuple._1.toDouble} ${tuple._2.toString}")

  /**
    * An integer value with an attached dimension
    * {{{dimensionedInt ::= integer dimension}}}
    *
    * @return A string representing the integer with the attached dimension
    */
  def dimensionedInt: Parser[String] = integer ~ dimension ^^ (tuple => s"${tuple._1.toInt} ${tuple._2.toString}")

  /**
    * A value with an attached dimension
    * {{{dimensionedValue ::= dimensionedDouble | dimensionedInt}}}
    *
    * @return A string representing the dimensioned value
    */
  def dimensionedNumber: Parser[String] = dimensionedDouble | dimensionedInt

  //
  // ooo===------< NUMBERS AND RANGES >------===ooo

  /**
    * A uniform random number between the start and end, expressed as `rstart:end`. For example, r0.1:2.2 would
    * return a random number in the interval [0.1, 2.2]
    * {{{random ::= r double ":" double}}}
    *
    * @return A random number in the specified interval [start, end]
    */
  def random: Parser[Double] = "r" ~ number ~ ":" ~ number ^^ {
    case "r" ~ start ~ ":" ~ end => start + math.random() * (end - start)
  }

  /**
    * A range of integers expressed as `start:end:interval` that is expanded into a set of integers
    * {{{range ::= integer ":" integer ":" integer.}}}
    *
    * @return A set of integer expanded form the range
    */
  def range: Parser[Set[Int]] = integer ~ ":" ~ integer ~ ":" ~ integer ^^ {
    case start ~ ":" ~ end ~ ":" ~ interval => (start to end by interval).toSet
  }

  /**
    * A number that is either a double or an integer, returned as a double
    * {{{number ::= double | integer.}}}
    *
    * @return A [[Parser]] holding a double value
    */
  def number: Parser[Double] = double | integer ^^ (_.toDouble)

  /**
    * An integer represented as a sequence of digits or in scientific notation.
    * {{{integer ::= regex(([+-]?\d+){1}([eE]{1}[+]?\d+)).}}}
    *
    * @return An integer
    */
  def integer: Parser[Int] =
    """([+-]?\d+){1}([eE]{1}[+]?\d+)?""".r ^^ (_.toInt)

  /**
    * A double represented as a floating point or in scientific notation
    * {{{double ::= regex(([+-]?\d*\.\d*){1}([eE]{1}[+-]?\d+)?|([+-]?\d*){1}([eE]{1}[-]?\d+)).}}}
    *
    * For example, -1.9, 0.0532, -1.654e-10, 1.5534E5, 1e-9, etc
    *
    * @return a double
    */
  def double: Parser[Double] =
    """([+-]?\d*\.\d*){1}([eE]{1}[+-]?\d+)?|([+-]?\d*){1}([eE]{1}[-]?\d+)""".r ^^ (_.toDouble)

  /**
    * A boolean
    * {{{boolean ::= regex(f|false|t|true)}}}
    *
    * @return a boolean
    */
  def boolean: Parser[Boolean] =
    """f|false|t|true""".r ^^ {
      case "f" => false
      case "false" => false
      case "t" => true
      case "true" => true
    }

  //
  // ooo===------< ID >------===ooo
  //

  /**
    * An ID. Similar to a key but can contain `-` following anywhere after the first character
    * {{{id ::= regex([a-zA-Z0-9]+[\-]*[a-zA-Z0-9]).}}}
    *
    * @return A string representing the ID
    */
  def id: Parser[String] =
    """[a-zA-Z0-9]+[\-_]*[a-zA-Z0-9]*""".r

  //
  // ooo===------< NETWORK ADDRESS >------===ooo
  //
  def host: Parser[String] =
    """((\d{1,3}\.){3}\d{1,3})|([0-9a-fA-F:]+)|([a-zA-Z\-_\.]+)""".r

}

object DnaParser {

  /**
    * Apply function that acts as a constructor for the parse.
    * @param validateReference (Optional) set to `true` to validate the references and treat any
    *                          reference violations as error; `false` (default) to skip the reference checks
    * @return A [[DnaParser]]
    */
  def apply(validateReference: Boolean = false): DnaParser = new DnaParser(validateReference)

  /**
    * Construct a location description based on the specified coordinate system type and the tuple of projections
    *
    * @param coordinateType The coordinate system type (i.e. cartesian, cylindrical, spherical)
    * @param px1            The first projection (in cartesian this would be `x`, in cylindrical this would be `r`)
    * @param px2            The second projection (in cartesian this would be `y`, in cylindrical this would be `φ`)
    * @param px3            The third projection (in cartesian this would be `z`, in cylindrical this would be `z`)
    * @return The location description describing the location
    */
  private def asLocation(coordinateType: String, px1: (Int, String), px2: (Int, String), px3: (Int, String)) = {
    val px = Map(px1._1 -> px1._2, px2._1 -> px2._2, px3._1 -> px3._2)
    val point: (String, String, String) = coordinateType match {
      case CARTESIAN.name => (
        Length(px(1)).getOrElse(Microns(px(1).toDouble)).toString,
        Length(px(2)).getOrElse(Microns(px(2).toDouble)).toString,
        Length(px(3)).getOrElse(Microns(px(3).toDouble)).toString
      )

      case CYLINDRICAL.name => (
        Length(px(1)).getOrElse(Microns(px(1).toDouble)).toString,
        Angle(px(2)).getOrElse(Radians(px(2).toDouble)).toString,
        Length(px(3)).getOrElse(Microns(px(3).toDouble)).toString
      )

      case SPHERICAL.name => (
        Length(px(1)).getOrElse(Microns(px(1).toDouble)).toString,
        Angle(px(2)).getOrElse(Radians(px(2).toDouble)).toString,
        Angle(px(3)).getOrElse(Radians(px(3).toDouble)).toString
      )
    }
    new LocationDescription(coordinateType = coordinateType, point = point)
  }

  /**
    * Constructs the weight-decay description from the map generated by the parser
    *
    * @param weightDecay The map holding the key-value pairs describing the weight-decay function
    * @return the weight-decay description from the map generated by the parser
    */
  private def asWeightDecay(weightDecay: Map[String, Any]): WeightDecayDescription = {

    val decayParams = weightDecay(DECAY_TYPE.name).toString match {
      case EXPONENTIAL.name =>
        ExponentialDecayParams(weightDecay(DECAY_HALF_LIFE.name).asInstanceOf[Time])

      case ZERO.name => NoDecayParams()
    }
    new WeightDecayDescription(decayParams)
  }

  /**
    * Constructs the weight-limit description from the map generated by the parser
    *
    * @param weightLimiter The map holding the key-value pairs describing the weight-limiter function
    * @return the weight-limit description from the map generated by the parser
    */
  private def asWeightLimiter(weightLimiter: Map[String, Any]): WeightLimitDescription = {

    val limiterParams = weightLimiter(LIMITER_TYPE.name).toString match {
      case BOUNDED.name => BoundedParams(
        weightLimiter(LOWER_BOUND.name).toString.toDouble,
        weightLimiter(UPPER_BOUND.name).toString.toDouble
      )
      case UNBOUNDED.name => UnboundedParams()
    }
    new WeightLimitDescription(limiterParams)
  }

  /**
    * Constructs the signal-release probability description from the map generated by the parser
    *
    * @param synapseTiming The map holding the key-value pairs describing the synapse timing function
    * @return the signal-release probability description
    */
  private def asSynapseTiming(synapseTiming: Map[String, Any]): SignalReleaseProbabilityDescription = {
    val facilitation = SignalTimingFunction(
      synapseTiming(FACILITATION_BASE.name).asInstanceOf[Double],
      synapseTiming(FACILITATION_MAGNITUDE.name).asInstanceOf[Double],
      synapseTiming(FACILITATION_TIME_CONSTANT.name).asInstanceOf[Time]
    )
    val depletion = SignalTimingFunction(
      synapseTiming(DEPLETION_BASE.name).asInstanceOf[Double],
      synapseTiming(DEPLETION_MAGNITUDE.name).asInstanceOf[Double],
      synapseTiming(DEPLETION_TIME_CONSTANT.name).asInstanceOf[Time]
    )
    SignalReleaseProbabilityDescription(facilitation, depletion)
  }

  /**
    * Constructs the learning-function description from the map generated by the parser
    *
    * @param stdp The map holding the key-value pairs describing the spike-time-dependent plasticity function
    * @return the learning-function description from the map generated by the parser
    */
  private def asLearningFunction(learningType: Symbol, stdp: Map[String, Any]): Either[String, (String, LearningFunctionDescription)] = {
    try {
      //      val learningFunctionName = stdp(LEARNING_TYPE.name).toString
      //      val learningParams = learningFunctionName match {
      val learningParams = learningType match {
        case STDP_HARD =>
          StdpHardLimitLearningParams(
            inhibitionAmplitude = stdp(INHIBITION_AMPLITUDE.name).asInstanceOf[Double],
            inhibitionPeriod = stdp(INHIBITION_PERIOD.name).asInstanceOf[Time],
            excitationAmplitude = stdp(EXCITATION_AMPLITUDE.name).asInstanceOf[Double],
            excitationPeriod = stdp(EXCITATION_PERIOD.name).asInstanceOf[Time]
          )

        case STDP_SOFT =>
          StdpSoftLimitLearningParams(
            inhibitionAmplitude = stdp(INHIBITION_AMPLITUDE.name).asInstanceOf[Double],
            inhibitionPeriod = stdp(INHIBITION_PERIOD.name).asInstanceOf[Time],
            excitationAmplitude = stdp(EXCITATION_AMPLITUDE.name).asInstanceOf[Double],
            excitationPeriod = stdp(EXCITATION_PERIOD.name).asInstanceOf[Time]
          )

        case STDP_ALPHA =>
          StdpAlphaLearningParams(
            baseline = stdp(ALPHA_BASELINE.name).asInstanceOf[Double],
            learningRate = stdp(ALPHA_LEARNING_RATE.name).asInstanceOf[Double],
            timeConstant = stdp(ALPHA_TIME_CONSTANT.name).asInstanceOf[Time]
          )

        case NO_LEARNING => NoLearningParams()
      }
      Right((learningType.name, new LearningFunctionDescription(learningParams)))
      //      Right((learningFunctionName, new LearningFunctionDescription(learningParams)))
    } catch {
      case e: NoSuchElementException => Left(s"learning function parameter missing; ${e.getMessage}; $stdp")
    }
  }

  /**
    * Constructs the neuron description from the map generated by the parser
    *
    * @param params The map holding the key-value pairs describing the neuron
    * @return the neuron description from the map generated by the parser
    */
  private def asNeuron(params: Map[String, Any]): Either[String, (String, NeuronDescription)] = {
    try {
      val neuronId = params(NEURON_ID.name).toString
      val description = new NeuronDescription(
        neuronId = neuronId,
        groupId = params(NEURON_GROUP.name).asInstanceOf[String],
        inhibitor = params(INHIBITION.name).asInstanceOf[Boolean],
        refractoryPeriod = params(REFRACTORY_PERIOD.name).asInstanceOf[Time],
        baseRefractoriness = params(BASE_REFRACTORINESS.name).asInstanceOf[MagneticFlux],
        minMembranePotential = params(MIN_MEMBRANE_POTENTIAL.name).asInstanceOf[ElectricPotential],
        membranePotentialDecayHalfLife = params(MEMBRANE_POTENTIAL_DECAY.name).asInstanceOf[Time],
        membranePotentialRiseHalfLife = params(MEMBRANE_POTENTIAL_RISE.name).asInstanceOf[Time],
        membranePotentialNoise = params(MEMBRANE_POTENTIAL_NOISE.name).asInstanceOf[ElectricPotential],
        weightNoiseMagnitude = params(WEIGHT_NOISE.name).asInstanceOf[Double],
        neuronSpecificParams = neuronSpecificParams(params),
        weightDecayDescription = params(WEIGHT_DECAY_FUNCTION.name).asInstanceOf[WeightDecayDescription],
        weightLimitDescription = params(WEIGHT_LIMITER_FUNCTION.name).asInstanceOf[WeightLimitDescription],
        synapseTimingDescription = params(SIGNAL_RELEASE_PROBABILITY.name).asInstanceOf[SignalReleaseProbabilityDescription],
        locationDescription = params(LOCATION.name).asInstanceOf[LocationDescription],
        spikePotential = params(SPIKE_POTENTIAL.name).asInstanceOf[ElectricPotential],
        conductanceSpeed = params(CONDUCTANCE_SPEED.name).asInstanceOf[Velocity],
        intrinsicPlasticityBase = params(INTRINSIC_PLASTICITY_BASE.name).asInstanceOf[ElectricPotential],
        intrinsicPlasticityLearningRate = params(INTRINSIC_PLASTICITY_LEARNING_RATE.name).asInstanceOf[ElectricPotential],
        intrinsicPlasticityDecayHalfLife = params(INTRINSIC_PLASTICITY_DECAY_HALF_LIFE.name).asInstanceOf[Time]
      )

      Right((neuronId, description))

    } catch {
      case e: NoSuchElementException => Left(s"neuron parameter missing; ${e.getMessage}; $params")
    }
  }

  /**
    * Pulls the neuron-specified parameters from the params
    *
    * @param params The parsed parameters
    * @return A [[NeuronSpecificParams]] instance
    */
  private def neuronSpecificParams(params: Map[String, Any]): NeuronSpecificParams =
    params(NEURON_TYPE.name).toString match {
      case MONOSTABLE_INTEGRATOR.name =>
        MonostableIntegratorParams(params(SPIKE_THRESHOLD.name).asInstanceOf[ElectricPotential])

      case BISTABLE_INTEGRATOR.name =>
        BistableIntegratorParams(
          limitCycleThreshold = params(LIMIT_CYCLE_THRESHOLD.name).asInstanceOf[ElectricPotential],
          restingStateThreshold = params(RESTING_STATE_THRESHOLD.name).asInstanceOf[ElectricPotential],
          tonicFireRate = params(TONIC_FIRE_RATE.name).asInstanceOf[Frequency]
        )
    }

  /**
    * Constructs a list of the connections for the network
    *
    * @param params The parameters describing the connections in the network
    * @return A list of connection description instances
    */
  private def asConnection(params: Map[String, Any]): List[Either[String, ConnectionDescription]] = {
    try {
      params(PRE_SYNAPTIC_NEURON.name).asInstanceOf[Set[String]]
        .flatMap(preSyn => params(POST_SYNAPTIC_NEURON.name).asInstanceOf[Set[String]]
          .map(postSyn => new ConnectionDescription(
            preSynapticNeuronId = preSyn,
            postSynapticNeuronId = postSyn,
            initialWeight = params(INITIAL_CONNECTION_WEIGHT.name).toString.toDouble,
            equilibriumWeight = params.getOrElse(EQUILIBRIUM_CONNECTION_WEIGHT.name, params(INITIAL_CONNECTION_WEIGHT.name)).toString.toDouble,
            learningFunctionName = params(LEARNING_FUNCTION_NAME.name).toString
          )).toList
        ).toList.map(description => Right(description))
    } catch {
      case e: NoSuchElementException => List(Left(s"connection parameter missing; ${e.getMessage}; $params"))
    }
  }

  /**
    * Constructs a group description from the parser parameters.
    *
    * @param params The parameters describing the group
    * @return Either a pair holding the group ID and the associated description, or a failure message
    */
  private def asGroup(params: Map[String, Any]): Either[String, (String, GroupDescription)] = {
    try {
      val groupId = params(GROUP_ID.name).asInstanceOf[String]
      val remoteParams = (params.get(HOST.name), params.get(PORT.name))
      remoteParams match {
        // both host and port are provided -> remote group
        case (Some(host), Some(port)) =>
          Right((
            groupId,
            new GroupDescription(groupId = groupId, params = RemoteGroupParams(host.asInstanceOf[String], port.asInstanceOf[Int]))
          ))

        // neither host nor port are provided -> local group
        case (None, None) =>
          Right((groupId, new GroupDescription(groupId = groupId, params = LocalGroupParams())))

        // one of (host, port) is provided, but not the other -> invalid group
        case _ =>
          Left(s"Invalid group specification. Must supply both (host, port) for remote group, or none for a local group; params: $params")
      }
    } catch {
      case e: NoSuchElementException => Left(s"group ID parameter missing; ${e.getMessage}; $params")
    }
  }

  /**
    * Converts the parameter to a set of string. If the parameter is a set of strings, then returns that.
    * Otherwise converts the `name` to a string and adds it to the specified set
    *
    * @param name The name of set or a set
    * @param set  The set to which to add the name
    * @return a set of string
    */
  private def asSet(name: Any, set: Set[String]): Set[String] =
    name match {
      case names: Iterable[Any] => names.flatMap(element => asSet(element, set)).toSet
      case _ => set + name.toString
    }
}



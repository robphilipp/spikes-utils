package com.digitalcipher.spiked.construction.description

/**
  * Created by rob on 1/22/17.
  */
case class ConnectionDescription(preSynapticNeuronId: String,
                                 postSynapticNeuronId: String,
                                 initialWeight: Double,
                                 equilibriumWeight: Double,
                                 learningFunctionName: String) {

  override def toString: String =
    s"pre_synaptic_neuron_id=$preSynapticNeuronId; " +
      s"post_synaptic_neuron-id=$postSynapticNeuronId; " +
      s"initial_connection_weight=$initialWeight; " +
      s"learning_function_name=$learningFunctionName"
}

object ConnectionDescription {

  val CONNECTIONS = 'CON
  val PRE_SYNAPTIC_NEURON = 'prn
  val POST_SYNAPTIC_NEURON = 'psn
  val INITIAL_CONNECTION_WEIGHT = 'cnw
  val EQUILIBRIUM_CONNECTION_WEIGHT = 'eqw
  val LEARNING_FUNCTION_NAME = 'lrn

  def fragment(description: ConnectionDescription): String =
    s"(" +
      s"${PRE_SYNAPTIC_NEURON.name}=${description.preSynapticNeuronId}, " +
      s"${POST_SYNAPTIC_NEURON.name}=${description.postSynapticNeuronId}, " +
      s"${INITIAL_CONNECTION_WEIGHT.name}=${description.initialWeight}, " +
      s"${EQUILIBRIUM_CONNECTION_WEIGHT.name}=${description.equilibriumWeight}, " +
      s"${LEARNING_FUNCTION_NAME.name}=${description.learningFunctionName}" +
      s")"

  def fragment(descriptions: List[ConnectionDescription]): String =
    s"${CONNECTIONS.name}=[${descriptions.map( description => fragment(description)).mkString(", ")}]"
}

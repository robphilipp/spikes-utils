package com.digitalcipher.spiked.construction.description

/**
  * The network description that holds the [[Map]] of the neuron descriptions, the [[List]] of the connection descriptions,
  * and the [[Map]] of the learning function descriptions
  *
  * @param neurons           The neuron descriptions
  * @param connections       The connection descriptions
  * @param learningFunctions The learning functions
  */
case class NetworkDescription(groups: Map[String, GroupDescription],
                              neurons: Map[String, NeuronDescription],
                              connections: List[ConnectionDescription],
                              learningFunctions: Map[String, LearningFunctionDescription]) {

  /**
    * @return `true` if the neurons or learning functions are empty; `false` otherwise
    */
  def isEmpty: Boolean = neurons.isEmpty || learningFunctions.isEmpty
}

/**
  * Companion object to the network description
  */
object NetworkDescription {

  /**
    * Creates the DNA fragment for the specified network description
    *
    * @param description The description of the network
    * @return A [[String]] containing the DNA fragrment describing the network
    */
  def fragment(description: NetworkDescription): String =
    s"(" +
      s"${GroupDescription.fragment(description.groups)}, " +
      s"${NeuronDescription.fragment(description.neurons)}, " +
      s"${ConnectionDescription.fragment(description.connections)}, " +
      s"${LearningFunctionDescription.fragment(description.learningFunctions)}" +
      s")"
}

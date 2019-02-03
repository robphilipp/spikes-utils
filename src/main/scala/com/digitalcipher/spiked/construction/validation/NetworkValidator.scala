package com.digitalcipher.spiked.construction.validation

import com.digitalcipher.spiked.construction.description.NetworkDescription

/**
  * Functions for validating the references in the network description. For example, validates
  * that the connections refer to only existing neurons and learning functions, and that
  * neurons refer only to existing groups.
  */
object NetworkValidator {

  /**
    * Runs through all the validations, collecting the errors, and returning the errors if there are
    * any. If there are no errors, then returns the network description
    * @param description The network description
    * @return Either the error messages if any reference violations occurred, or the network description
    */
  def validateReferences(description: NetworkDescription): Either[Seq[String], NetworkDescription] = {
    // collect all the missing references
    val missingRefs = validateConnectionReferences(description).left.getOrElse(Seq()) ++
      validateGroupReferences(description).left.getOrElse(Seq())

    // if there are missing references, then return them, otherwise return the original network description
    if(missingRefs.nonEmpty) {
      Left(missingRefs)
    } else {
      Right(description)
    }
  }

  /**
    * Validates that all the neurons and learning functions referenced in the connections
    * exist in the network description.
    * @param description The network description
    * @return A validation result with any missing references
    */
  def validateConnectionReferences(description: NetworkDescription): Either[Seq[String], NetworkDescription] = {
    val neuronIds = description.neurons.keySet
    val learningFunctions = description.learningFunctions.keySet

    val missing: Seq[String] = description.connections
      .filter(connection => !neuronIds.contains(connection.preSynapticNeuronId))
      .map(connection => s"Pre-synaptic neuron '${connection.preSynapticNeuronId}' not found in neurons; (" +
        s"prn=${connection.preSynapticNeuronId}, " +
        s"psn=${connection.postSynapticNeuronId}, " +
        s"cnw=${connection.initialWeight}, " +
        s"eqw=${connection.equilibriumWeight}, " +
        s"lrn=${connection.learningFunctionName}" +
        s")"
      ) ++
      description.connections
        .filter(connection => !neuronIds.contains(connection.postSynapticNeuronId))
        .map(connection => s"Post-synaptic neuron  '${connection.postSynapticNeuronId}' not found in neurons; (" +
          s"prn=${connection.preSynapticNeuronId}, " +
          s"psn=${connection.postSynapticNeuronId}, " +
          s"cnw=${connection.initialWeight}, " +
          s"eqw=${connection.equilibriumWeight}, " +
          s"lrn=${connection.learningFunctionName}" +
          s")"
        ) ++
      description.connections
        .filter(connection => !learningFunctions.contains(connection.learningFunctionName))
        .map(connection => s"Learning function '${connection.learningFunctionName}' not found in learning functions; (" +
          s"prn=${connection.preSynapticNeuronId}, " +
          s"psn=${connection.postSynapticNeuronId}, " +
          s"cnw=${connection.initialWeight}, " +
          s"eqw=${connection.equilibriumWeight}, " +
          s"lrn=${connection.learningFunctionName}" +
          s")")

    if(missing.isEmpty) Right(description) else Left(missing)
  }

  /**
    * Validates that the neurons only refer to existing groups
    * @param description The network description
    * @return A sequence of missing references, or the network description
    */
  def validateGroupReferences(description: NetworkDescription): Either[Seq[String], NetworkDescription] = {
    val groupIds = description.groups.keySet

    val missing: Seq[String] = description.neurons.values
      .filter(neuron => !groupIds.contains(neuron.groupId))
      .map(neuron => s"Group '${neuron.groupId}' not found in groups; (" +
        s"nid=${neuron.neuronId}, " +
        s"grp=${neuron.groupId}, " +
        s"${neuron.neuronSpecificParams.fragment}, ...)"
      )
      .toSeq

    if(missing.isEmpty) Right(description) else Left(missing)
  }
}

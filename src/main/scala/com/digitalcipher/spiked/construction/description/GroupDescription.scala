package com.digitalcipher.spiked.construction.description

/**
  * Describes the neuron groups. Neurons can be organized into groups, and a group will be assigned to a particular
  * host and port.
  *
  * Created by rob on 7/15/17.
  */
case class GroupDescription(groupId: String, params: GroupParams)

object GroupDescription {
  val GROUPS = 'GRP

  val GROUP_ID = 'gid
  val HOST = 'hst
  val PORT = 'prt

  /**
    * Creates the DNA fragment for the neuron group
    * @param description The neuron group description
    * @return DNA fragment representing the neuron group
    */
  def fragment(description: GroupDescription): String =
    s"(${GROUP_ID.name}=${description.groupId}${description.params.fragment})"

  /**
    * Creates DNA fragment for the collection of the neurons
    * @param descriptions The collection of neuron descriptions
    * @return DNA fragment for the collection of the neurons
    */
  def fragment(descriptions: Map[String, GroupDescription]): String =
    s"${GROUPS.name}=[${descriptions.map( entry => fragment(entry._2)).mkString(", ")}]"
}

/**
  * The learning-type specific parameters
  */
trait GroupParams {
  def fragment: String
}

case class RemoteGroupParams(host: String, port: Int) extends GroupParams {
  import com.digitalcipher.spiked.construction.description.GroupDescription._

  override def fragment: String = s", ${HOST.name}=$host, ${PORT.name}=$port"
}

case class LocalGroupParams() extends GroupParams {
  override def fragment: String = ""
}

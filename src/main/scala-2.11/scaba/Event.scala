package scaba

/**
 * Created by CptChaos on 6/2/2015.
 */
case class Event(n: Node, state:Int) {
  def |(e: QueryCondition) = {
    TableLookupQuery(this, e)
  }

  def &(e: Event) = {
    QueryCondition(Seq(this, e))
  }

  override def toString = n.name + "<<" + n.states(state)
}


case class QueryCondition(evidences: Seq[Event]) {
  def &(e: Event) = {
    QueryCondition(evidences.toList :+ e)
  }

  override def toString = {
    evidences.mkString(" & ")
  }
}
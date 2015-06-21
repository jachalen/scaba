/**
 * ScaBa - A Scala implementation for Discrete Baysian (Belief) Networks
 *
 * (c) 2015 Jan Charles Lenk
 *
 * Licensed under LGPL
 *
 *
 * @author jan.charles.lenk@gmail.com
 */
package scaba


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
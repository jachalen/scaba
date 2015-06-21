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


class Query(val q: QueryCondition)

case class TableLookupQuery(w: Event, override val q: QueryCondition) extends Query(q) {
  override def toString = {
    w + "|" + q
  }

}

case class InferenceQuery(w: Node, override val q: QueryCondition) extends Query(q) {
  override def toString = w + " | " + q
}

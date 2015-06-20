package de.scaba

/**
 * Created by CptChaos on 6/2/2015.
 */
class Query(val q: QueryCondition)

case class TableLookupQuery(w: Event, override val q: QueryCondition) extends Query(q) {
  override def toString = {
    w + "|" + q
  }

}

case class InferenceQuery(w: Node, override val q: QueryCondition) extends Query(q) {
  override def toString = w + " | " + q
}

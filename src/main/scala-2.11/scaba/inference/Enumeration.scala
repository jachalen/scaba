package scaba.inference

import scaba._

//import BBN._

object Enumeration extends InferenceAlg {
  def apply( bbn:BBN, node:Node, evidence:Event*):List[Tuple2[Symbol,Double]] = {
    //println("Enumerate All")
    enum_ask(bbn, node, evidence:_*)
  }
    
  def enum_ask(bbn:BBN, node: Node, evidence: Event*):List[Tuple2[Symbol, Double]] = {
    var ret = List[Tuple2[Symbol, Double]]()
    val ev = evidence.toList
    val nodes_sorted = bbn.nodes.values.toList.sortWith(!bbn.isParent(_, _))
    node.states.foreach(state =>
      ret = (state, enum_all(bbn, nodes_sorted, (node << state) :: ev)) :: ret)
    Util.normalize( ret.reverse)
  }

  def enum_all(bbn:BBN, vars: List[Node], evidence: List[Event]): Double = {
    if (vars.isEmpty) {
      1.0
    } else {
      val Y = vars.head
      val parentsY = evidence.filter(e => bbn.isParent(Y, e.n))
      if (evidence.exists(e => e.n == Y)) {
        val ev = evidence.find(e => e.n == Y).head
        bbn.P(ev | parentsY) * enum_all(bbn, vars.tail, evidence)
      } else {
        val sum = Y.states.map( state => 
          bbn.P(Y << state | parentsY) * enum_all(bbn, vars.tail, (Y << state) :: evidence)).sum
        sum
      }
    }
    
  }
  
  implicit def seqEv2QueryCondition(l: List[Event]): QueryCondition = QueryCondition(l)
}
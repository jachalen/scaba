package scaba

import scaba.inference.Enumeration

import scala.collection.mutable

/**
 * Created by CptChaos on 9/4/2015.
 */
trait ScabaAPI {
  /**
   * A Function type for inference algorithms.
   * @param BBN a Bayesian Network
   * @param Node the target node for inference
   * @param Event
   *
   */
  type InferenceAlg = (BBN,Node,Event*)=>List[Tuple2[Symbol,Double]]
  implicit def sym2Condition(s: Symbol)(implicit bbn:BBN): ConditionDeclaration = ConditionDeclaration(List(bbn.getNode(s)))
  implicit def evidence2Query(e: Event) = TableLookupQuery(e, QueryCondition(Seq.empty))
  implicit def evidence2QueryCondition(e: Event) = QueryCondition(Seq(e))
  implicit def sym2Node(s: Symbol)(implicit bbn:BBN): Node = bbn.getNode(s)

  //implicit def event2Set(e:Event):Set[Event] = Set(e)

  implicit class tableentry( eventSet:Iterable[Event]) {
    def --> (values:Node=>Seq[Double]):(Set[Event],Node=>Seq[Double]) = (eventSet.toSet,values)
    def --> (values:Double*):(Set[Event],Node=>Seq[Double]) = this --> %(values: _*)
  }

  implicit class tableentry2(qc:QueryCondition) extends tableentry(qc.evidences.toSet)

  implicit class tableentrysingle( e:Event) extends tableentry(Set(e))


  implicit def nodeInfo2Map(t: NodeInfo): mutable.Map[Set[Event], Seq[Double]] = t.m

  def P(n: Node)(implicit bbn:BBN) = bbn.P(n)
  def P(query: InferenceQuery)(implicit bbn:BBN, alg:InferenceAlg=Enumeration) : List[Tuple2[Symbol, Double]] = bbn.P(query)(alg)
  def P(c: Conditional)(implicit bbn:BBN) = bbn.P(c)
}

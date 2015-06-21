import scaba.inference.Enumeration
import scala.collection.mutable

/**
 * ScaBa - A Scala implementation for Discrete Baysian (Belief) Networks
 *
 * (c) 2015 Jan Charles Lenk
 *
 * Licensed under LGPL
 *
 *
 * @author jan.charles.lenk@gmail.com
 *
 *
 *
 * Usage on the console:
 *
 * //necessary imports
 * import de.scaba._
 *
 * //Set the BBN to example
 * implicit val bbn = example.Alarm
 * //Execute Query
 * P('Burglary | 'MaryCalls << 'True & 'JohnCalls << 'True)
 * //Define a new BBN with two nodes
 * val test = new BBN
 * implicit val bbn = test
 * 'A:=('t,'f)
 * 'B:=('t,'f)
 * P('A)++=(§ -> %(0.2,0.8))
 * P('B | 'A) ++= ((§('A<<'t)-> %(0.1,0.9)),(§('A<<'f)-> %(0.6,0.4)))
 * P('B | 'A) += (1 -> %(0.2,0.8))
 * P('A | 'B << 't)
 */
package object scaba {
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

  implicit def nodeInfo2Map(t: NodeInfo): mutable.Map[Set[Event], Seq[Double]] = t.m

  def P(n: Node)(implicit bbn:BBN) = bbn.P(n)
  def P(query: InferenceQuery)(implicit bbn:BBN, alg:InferenceAlg=Enumeration) : List[Tuple2[Symbol, Double]] = bbn.P(query)(alg)
  def P(c: Conditional)(implicit bbn:BBN) = bbn.P(c)

}

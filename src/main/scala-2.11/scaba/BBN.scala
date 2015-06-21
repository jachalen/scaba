package scaba
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


import scala.collection.mutable
import scaba.inference._

/**
 * Main class for Bayesian Belief Networks (BBNs)
 */
class BBN {
  implicit val bbn = this
  var name:String =null
  var nodes = mutable.Map[Symbol, Node]()


  def P(n: Node) = {
    if (!n.table.isEmpty) n.table.clear()
    n.parents = Nil
    n.updateTableEntries
    Table(n)
  }

  def P(c: Conditional) = {
    if ( c.n.parents.toSet!=c.c.parents.toSet) {
      c.n.table.clear
    }
    
    
    c.n.parents = c.c.parents.toList
    c.n.updateTableEntries
    Table(c.n)
  }

  def P(query: InferenceQuery)(implicit alg:InferenceAlg=Enumeration): List[Tuple2[Symbol, Double]] = {
    alg(this, query.w, query.q.evidences: _*)
  }

  def P(q: TableLookupQuery): Double = {
    q.w.n.table(q.q.evidences.toSet)(q.w.state)
  }

  def Table(n: Node) = NodeInfo(n, n.table)

  def getNode(s: Symbol): Node = {
    if (nodes.contains(s)) {
      nodes(s)
    } else {
      val n = new Node( this, s, null)
      nodes += Tuple2(s,n)

      n
    }
  }

  def isParent(n: Node, n2: Node) = {
    n.parents.contains(n2)
  }
  
  def getChildren( n:Node) = {
    nodes.values.filter( _.parents.contains(n))
  }

  def defn(s: Symbol) = getNode(s)

  override def toString = this.nodes.values.map( _.probabilityString).mkString("\n")
  
  def setName( s:String):Unit = name = s
  
  def toXML = 
    <network>
			<name>{name}</name>
			{nodes.values.map( _.toXML)}
  		{nodes.values.map( _.toXML2)}
		</network>

}


object BBN  {

  
  
  def fromXML( n:scala.xml.Node, bbnFromXML: (scala.xml.Node,BBN)=>Unit, nodeFromXML: (scala.xml.Node,BBN)=>Node):BBN = {
    val bbn = new BBN()
    bbn.name = (n \"name").text
    if ( bbnFromXML!=null) {
      bbnFromXML(n, bbn)
    }
      
      (n \ "variable").foreach(nodeFromXML(_,bbn))
      (n \ "definition").foreach(Node.fromXML2(_, bbn))
    bbn
  }
  
  def fromXML( n:scala.xml.Node, nodeFromXML: (scala.xml.Node,BBN)=>Node):BBN = fromXML(n,null,nodeFromXML)
  
  def fromXML( n:scala.xml.Node):BBN = fromXML(n, Node.fromXML)
}





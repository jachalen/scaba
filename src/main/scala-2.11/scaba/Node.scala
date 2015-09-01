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

import scala.collection.mutable



class Node(val bbn: BBN, var name: Symbol, var states: List[Symbol]) {
  var parents: List[Node] = List[Node]()
  val table = mutable.Map[Set[Event], Seq[Double]]()

  def :=(s: Symbol*): Node = {
    this.states = s.toList
    updateTableEntries
    bbn.getChildren(this).foreach( _.updateTableEntries)
    this
  }

  def |(c: ConditionDeclaration): Conditional = {
    this.parents = c.parents.toList
    Conditional(this, c)
  }

  def |(c: QueryCondition): InferenceQuery = {
    InferenceQuery(this, c)
  }

  def <<(s: Symbol): Event = {
    if (states.contains(s)) Event(this, states.indexOf(s))
    else throw new IllegalArgumentException(s + " is not a state of " + this.name)
  }

  def ->(s: Symbol): Node = {
    if (!bbn.nodes.contains(s)) {
      bbn.nodes -= this.name
      this.name = s
      bbn.nodes += (this.name -> this)
    }
    this
  }

  override def toString = name.toString// + ":={" + (if (states != null) states.mkString(",")) + "}"

  def toXML: scala.xml.Elem =
    <variable type="nature">
      <name>{ name.toString.substring(1) }</name>
      {
        if (states != null) {
          for (s <- states) yield <outcome>{ s.toString.substring(1) }</outcome>
        }
      }
    </variable>

  def toXML2: scala.xml.Elem =
    <definition>
      <for>{ name.toString.substring(1) }</for>
      { for (p <- parents) yield <given>{ p.name.toString.substring(1) }</given> }
      <table>{ 
        val sigs=getRowSignatures
        val str = for (s<-sigs) yield (table(s).mkString(" "))
        str.mkString(" ")}</table>
			
    </definition>

  def getRowSignatures: List[Set[Event]] = {
    if ( this.parents.exists(_.states.size<2) || this.states.size<2) {
      List.empty[Set[Event]]
    }
    else if (this.parents.size == 0) {
      List(Set.empty[Event])
    } else {
      val par = this.parents.toList
      val initialList = List[List[Event]](List.empty[Event])
      def createList(list: List[List[Event]], rest: List[Node]): List[List[Event]] = {
        val result = Node.cross(list, rest.head)
        if (rest.tail.isEmpty) {
          result
        } else {
          createList(result, rest.tail)
        }
      }

      val z = createList(initialList, par)
      val f = for (zz <- z) yield (zz.toSet)
      f
    }

  }
  
  def getRowSignaturesAsList: List[List[Event]] = {
    if (this.parents.size == 0) {
      List(List.empty[Event])
    } else {
      val par = this.parents.toList
      val initialList = List[List[Event]](List.empty[Event])
      def createList(list: List[List[Event]], rest: List[Node]): List[List[Event]] = {
        val result = Node.cross(list, rest.head)
        if (rest.tail.isEmpty) {
          result
        } else {
          createList(result, rest.tail)
        }
      }

      createList(initialList, par)

    }

  }

  def updateTableEntries = {
    //println("UPDATE")
    //assert( this.parents.forall(_.states.size>1), "Cannot create table for "+this+" because parents lack states")
    //assert( this.states.size>1, "Cannot create table for "+this+" because it lacks states")

    if (this.parents.forall(_.states.size > 1) && this.states.size > 1) {
      val signatures: List[Set[Event]] = getRowSignatures
      val ptable = NodeInfo(this, this.table)
      for (sig <- signatures) {
        if (this.table.contains(sig)) {
          if (this.table(sig).size != this.states.size) {
            //ptable ++= (<>(sig.toList: _*) -> Uniform)
            ptable ++= sig -> Uniform
          }
        } else {
          //ptable ++= (<>(sig.toList: _*) -> Uniform)
          ptable ++= (sig -> Uniform)
        }
      }
    }
  }
  
  def probabilityString:String = "P("+this.name.toString +(if (this.parents.size>0) "|"+parents.map(_.name.toString).mkString("&")+")" else ")")

}

object Node {
  def fromXML(n: scala.xml.Node, babn: BBN): Node = {
    new Node(
      bbn = babn,
      name = Symbol((n \ "name").text),
      states = { (for (nn <- (n \ "outcome")) yield Symbol(nn.text)) }.toList) {
      babn.nodes += (name -> this)
    }

  }
  
  def fromXML2( n:scala.xml.Node, babn:BBN) = {
    val node = babn.getNode(Symbol((n \ "for").text))
    val parents = for (nn<- (n \"given")) yield (babn.getNode(Symbol(nn.text))) 
    node.parents ++= parents
    
    val tableEntries = (n \ "table").text.split(" ").map(_.toDouble)
    val step = node.states.size
    val sigs = node.getRowSignatures
    //println(sigs)
    //println(tableEntries.length)
    for ( i<-0 to sigs.length-1) {
      val index = i*step
      var values = List[Double]()
      val lim = index+step-1
      //println(index+"..."+lim)
      for ( i2<-index to lim) {
        
        values = values :+ tableEntries(i2)
      }
      node.table += ( sigs(i)->values)
    }
  }

//  def cross2(l: List[List[Event]], n: Node): List[List[Event]] = {
//    for (e <- l; i <- 0 until n.states.length) yield Event(n,i)::e
//  }
//  
//  def cross3(l: List[List[Event]], n: Node): List[List[Event]] = {
//    def traverse( list:List[List[Event]]):List[List[Event]] = {
//      val e = list.head
//      var r = List.empty[List[Event]]
//      for ( i <- 0 until n.states.length) {
//        r = (Event(n,i)::e):: r 
//      }
//      r ::: (if ( !list.tail.isEmpty) traverse(list.tail) else List.empty[List[Event]])
//    }
//    traverse( l)
//  }
  
  def cross(l: List[List[Event]], n: Node): List[List[Event]] = {
    def traverse( list:List[List[Event]]):List[List[Event]] = {
      val e = list.head

      def traverseStates(state:Int, toAppend:List[Event]):List[List[Event]]={
        val li= Event(n, state)::toAppend
        li :: (if ( state==0) {
          List.empty[List[Event]]
        } else {
          
          traverseStates( state-1,toAppend)
        })
      }
      
      traverseStates(n.states.length-1,e) ::: (if ( list.tail.isEmpty)
        List.empty[List[Event]] 
          else traverse(list.tail))
    }
    traverse( l)
  }

}

case class ConditionDeclaration(parents: Seq[Node]) {
  def &(n2: Node): ConditionDeclaration = {
    ConditionDeclaration(parents.toList :+ n2)
  }
}

case class Conditional(n: Node, c: ConditionDeclaration) {
  //  override def node = n
  //  override def parents = c.parents
}

case class NodeInfo(node: Node, m: mutable.Map[Set[Event], Seq[Double]]) {
  def ++=(kv: Tuple2[ Set[Event], Node=>Seq[Double]]*): NodeInfo = {
    val z = for (e <- kv) yield (e._1 -> e._2(node))
    m ++= (z)

    this
  }
  
  
  def +=( kv :Tuple2[Int, Node=>Seq[Double]]):NodeInfo = {
    m+=(node.getRowSignatures(kv._1) -> kv._2(node))
    this
  }
  
  

  override def toString = {
    def maxStringSize(n: Node): Int = {
      var maxSize = n.name.toString.length
      n.states.foreach(state =>
        if (state.toString.length > maxSize) { maxSize = state.toString.length })
      maxSize
    }

    val sizesParents: List[Int] = for (n <- node.parents) yield (maxStringSize(n))

    var s = "\n    "
    var lineTemplate = "(%2d)"
    for (index: Int <- 0 to sizesParents.size - 1) {
      s += " %" + sizesParents(index) + "s"
      lineTemplate += " %" + sizesParents(index) + "s"
    }

    for (index <- 0 to node.states.size - 1) {
      val size = if (node.states(index).toString.length > 5) node.states(index).toString.length else 5
      val decimalplaces = size-2
      s += " %" + size + "s"
      
      lineTemplate += " %"+ size +"." +decimalplaces+"f"
    }

    val header = (for (n <- node.parents) yield (n.name.toString)) ::: (for (s <- node.states) yield (s.toString))
    //println(header)
    var output = s.format(header: _*)
    val signatures = node.getRowSignatures
    for (index <- 0 to signatures.size - 1) {
      val sig = signatures(index)
      output += "\n"
      val entries: List[Any] = (for (s <- sig.toSeq.sortWith((e1,e2)=>node.parents.indexOf(e1)<node.parents.indexOf(e2))) yield (s.n.states(s.state))).toList

      output += lineTemplate.format((index :: entries ::: m(sig).toList): _*)
    }

    node.probabilityString+"\n"+output
  }
}


case class %(values: Double*) extends Function[Node, Seq[Double]] {
  def apply(n: Node) = values
}


object Uniform extends (Node => Seq[Double]) {
  def apply(node: Node) = List.fill[Double](node.states.size)(1.0 / node.states.size)
}

/*
case class <>(keys: Event*) extends Function0[Set[Event]] {
  val set = keys.toSet
  def apply() = set
  def ->(f: Node => Seq[Double]): Tuple2[() => Set[Event], Node => Seq[Double]] = (this, f)
}

object <> extends <>()
*/

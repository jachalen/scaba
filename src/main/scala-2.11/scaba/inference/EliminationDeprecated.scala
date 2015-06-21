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
package scaba.inference

import scaba._



/**
 * @author CptChaos
 *
 */
object EliminationDeprecated extends InferenceAlg {
  def apply(bbn: BBN, node: Node, evidence: Event*): List[Tuple2[Symbol, Double]] = {
    //println("Elimination")

    val x = eliminationAsk(bbn, node, evidence: _*)
    val r = x._1.toList.map(x => (x._1.toList.head, x._2)).sortWith(
      (a, b) => a._1.state < b._1.state).map(
        a => (a._1.n.states(a._1.state), a._2))
    Util.normalize(r)
  }

  /**
   * @param bbn
   * @param node
   * @param evidence
   * @return
   */
  def eliminationAsk(bbn: BBN, node: Node, evidence: Event*): Factor = {
    val ev = evidence.toList
    val nodes_sorted = bbn.nodes.values.toList.sortWith(bbn.isParent(_, _))
    //nodes_sorted = switchFirstTwo(nodes_sorted)
    val hidden = nodes_sorted.diff(List(node)).diff(ev.map(_.n))

    
    var factors = List[Factor]()
    for (n <- nodes_sorted) {
      factors = makeFactor(n, ev, bbn) :: factors
      //println( toString( factors))
      if (hidden.contains(n)) {
        factors = sumOut(n, factors, bbn)
      }
    }
    product(factors)
  }

  //  def tostring( f:Factor):String = {
  //    "<"+f._1.values.toList.mkString(",")+">"
  //  }
  //  
  //  def toString( f: List[Factor]) = "["+(for ( ff<-f) yield (tostring(ff))).mkString(",")+"]"

  //  def switchFirstTwo( l:List[Node]):List[Node] = {
  //    val first = l.head
  //    val sec = l.tail.head
  //    val rest = l.tail.tail
  //    sec :: first :: rest
  //  }



  def makeFactor(n: Node, e: List[Event], bbn: BBN): Factor = {
    var evidence = List[Event]()
    var vars = n :: n.parents 
    for (ev <- e) {
      if (vars.contains(ev.n)) {
        evidence = ev::evidence
      }
    }
    
    for (ev <- evidence) {
      if (vars.contains(ev.n)) {
        vars = vars diff List(ev.n)
      }
    }
    val factorE = filter2(bbn.Table(n), evidence)

    (factorE, vars.toSet)
  }

  def sumOut(n: Node, factors: List[Factor], bbn: BBN): List[Factor] = {
    var ret = List[Factor]()
    var mult = List[Factor]()
    factors.foreach(f => if (contains(f._1, n)) {
      mult = f :: mult
    } else {
      ret = f :: ret
    })

    val prod = product(mult)
    //println( "prod: "+tostring(prod))

    ret = sumOut(n, prod) :: ret
    //println( "sum:"+toString( ret))
    ret
  }

  def sumOut(n: Node, f: Factor): Factor = {
    val ff = for (i <- 0 to n.states.length - 1) yield (reduce(Event(n, i), f))
    doSum(ff)
  }

  def doSum(f: Seq[Factor]): Factor = {
    var ret = Map[Set[Event], Double]()
    for (k <- f.head._1.keys) {
      val sum = f.map( _._1(k)).sum
      //f.foreach(sum += _._1(k))
      ret += (k -> sum)
    }

    (ret, f.head._2)
  }

  def reduce(ev: Event, f: Factor): Factor = {
    val newNodes = f._2 - ev.n// diff Set(ev.n)
    val entries = f._1.filter(kv => kv._1.contains(ev))
    val entries2 = entries.map(kv => {
      val newSet = kv._1.filter(elem => newNodes.contains(elem.n))
      (newSet, kv._2)
    })

    (entries2, newNodes)
  }

  //def product(fl: List[Factor]): Factor = product(fl.head, fl.tail)

  def productO(fl: List[Factor]): Factor = {
    var p = fl.head
    fl.tail.foreach( f => p = product( p, f))
    p
  }
  
  def product(fl: List[Factor]): Factor = {
    if ( fl.tail.isEmpty) {
      fl.head
    } else {
      product(product( fl.head, fl.tail.head) :: fl.tail.tail)
    }
  }
  
  def product(f: Factor, fl: List[Factor]): Factor = {
    if (fl.isEmpty) {
      f
    } else {
      val prod = product(f, fl.head)
      product(prod, fl.tail)
    }
  }
  
  

  def product(f1: Factor, f2: Factor): Factor = {
    var s = List[Tuple2[Set[Event], Double]]()
    val vars = f1._2.union(f2._2)
    for (k1 <- f1._1.keys; k2 <- f2._1.keys) {
      val newkey = k1.union(k2)
      if (isValid(newkey)) {
        s = (newkey, f1._1(k1) * f2._1(k2)) :: s
      }
    }


    (s.toMap, vars)
  }
  
  

  def isValid(k: Set[Event]): Boolean = {
    val s = k.map(_.n)
    k.size == s.size
  }

  def contains(f: FactorEntries, node: Node): Boolean = {
    f.keys.exists(_.exists(_.n == node))
  }

  def filter2(f: FactorEntries, e: List[Event]): FactorEntries = {
    f.filter(kv => 
      e.forall(ev => 
        kv._1.contains(ev)))
  }

  type FactorEntries = Map[Set[Event], Double]
  type Factor = (FactorEntries, Set[Node])

  implicit def ptable2Factor2(pt: NodeInfo): FactorEntries = {
    val node = pt.node
    var ret = Map[Set[Event], Double]()
    for (key <- pt.node.getRowSignatures; i <- 0 to node.states.size - 1) {

      val newKey: Set[Event] = key + Event(node, i)
      //println(key+ "  "+i+"..."+newKey)
      ret += (newKey -> pt.m(key)(i))

    }

    ret
  }
}
package de.scaba.inference

import de.scaba._
import de.scaba.BBN._

case class Factor(vars: List[Node], keys: List[List[Event]]) {
  def this(vars: List[Node]) = { this(vars, Factor.createKeys(vars)) }

  val indexOffsets: Array[Int] = new Array(vars.length)

  private def  createIndexOffsets(nodes: List[Node], index:Int, offset:Int):Unit = {
    
    if ( !nodes.isEmpty) {
      indexOffsets(index)=offset
      createIndexOffsets( nodes.tail, index+1, offset*nodes.head.states.length)
    }
  }
  
  createIndexOffsets( vars, 0, 1)

  val values = new Array[Double](keys.length)//Factor.computeArraySize(vars))

  def getIndex(e: List[Event]):Int = {
    if ( e.isEmpty) {
      0
    } else {
      this(e.head.n)*e.head.state + getIndex( e.tail)
    }
  }

  def +=(t: Tuple2[List[Event], Double]): Unit = values(getIndex(t._1)) = t._2

  def set(e:List[Event], v:Double) =  values(getIndex(e)) = v
  
  def apply(e: List[Event]): Double = values(getIndex(e))

  def apply(n: Node): Int = {//if (vars.contains(n)) indexOffsets(vars.indexOf(n)) else 0
      def retrieve( l:List[Node], n:Node, index:Int) :Int = {
      if ( l.isEmpty) {
        0
      } else if ( l.head==n) {
        indexOffsets(index)
      } else if ( l.tail.isEmpty) {
        0
      } else {
        retrieve( l.tail, n, index+1)
      }
    }
    
    retrieve( vars, n, 0)
  }
  

    
  override def toString = {
    var ret = ""

    for (key <- keys) {
      ret += key.mkString(" & ") + " -> " + this(key) + "\n"
    }
    ret
  }
}

object Factor {

  def createKeys(par: List[Node]): List[List[Event]] = {
    val initialList = List[List[Event]](List.empty[Event])

    def createList(list: List[List[Event]], rest: List[Node]): List[List[Event]] = {
      val result = Node.cross(list, rest.head)
      if (rest.tail.isEmpty) {
        result
      } else {
        createList(result, rest.tail)
      }
    }

    if (par.isEmpty) {
      initialList
    } else {
      createList(initialList, par)
    }
  }

  def computeArraySize(vars: List[Node]): Int = {
    (for (v <- vars) yield (v.states.length)).product
  }


  def makeFactor(n: Node, e: List[Event], bbn: BBN): Factor = {
    val vars = n :: n.parents
    
    def appendEvidence( ev:List[Event]):List[Event] = {
      def addIfContains(vars: List[Node]):List[Event] = {
        if ( vars.isEmpty) {
          Nil
        } else {
           if ( vars.head==ev.head.n) {
            List(ev.head)
          } else {
            addIfContains( vars.tail)
          }
        }
      }
      if (ev.isEmpty) {
        List.empty[Event]
      } else {
        addIfContains( vars) ::: appendEvidence( ev.tail)
      }
    }
    
    val evidence = appendEvidence( e)

    
    def removeVars( ev:List[Event], vrs:List[Node]):List[Node] = {
      if ( ev.isEmpty) {
        vrs
      } else {
        if ( vrs.contains( ev.head.n)) {
          removeVars( ev.tail, Util.diff(List.empty[Node],vrs,ev.head.n))
        } else {
          removeVars( ev.tail, vrs)
        }
      }
        
    }

    val vars2 = removeVars(evidence, vars)

    val rows = n.getRowSignaturesAsList

    val rrows = Node.cross(rows, n)
    
    val used = rrows.filter(
      kv => evidence.forall(ev =>
        kv.contains(ev)))

    val factor = new Factor(vars2,used)

    for (entry <- used) {

      val value = bbn.P(entry.head | entry.tail)
      val key = entry
      factor.set( key, value)
    }

    factor
  }

//  def product(fl: List[Factor]): Factor = {
//    if (fl.tail.isEmpty) {
//      fl.head
//    } else {
//      product(product(fl.head, fl.tail.head) :: fl.tail.tail)
//    }
//  }

  def product2(fl: List[Factor]): Factor = {
     val vars = {

       def traverse( aggregation:List[Node], remaining:List[Factor]):List[Node]  = {
         if ( remaining.isEmpty) {
           aggregation
           
         } else {
           traverse( remaining.head.vars ::: aggregation, remaining.tail)
         }
       }

       traverse( fl.head.vars, fl.tail).distinct
     }
    val factor = new Factor(vars)
    factor.keys.foreach(k =>
      factor.set( k,
      {
        def product( key:List[Event], list:List[Factor]):Double = {
          if ( list.tail.isEmpty) {
            list.head(key)
          } else {
            list.head(key)*product(key,list.tail)
          }
        }
        product(k,fl)
      }
      )
      )
    factor
  }



  def sumOut(n: Node, factors: List[Factor], bbn: BBN): List[Factor] = {
    def traverse( factrs:List[Factor], mult:List[Factor], ret:List[Factor], n:Node):Tuple2[List[Factor],List[Factor]]= {
      if ( !factrs.isEmpty) {
       
        if ( contains( factrs.head.keys,n)) {
          
          traverse(factrs.tail, factrs.head :: mult, ret, n)
        } else {
          traverse(factrs.tail, mult, factrs.head :: ret,n)
        }
      } else {
        (mult,ret)
      }
    }
    val multret =traverse(factors,Nil,Nil,n)
    
    sumOut(n, product2(multret._1))::multret._2
  }
  
  def sumOut(n:Node, f:Factor):Factor = {
    val factor = new Factor(Util.diff( List.empty[Node], f.vars,n))//f.vars diff List(n))
    factor.keys.foreach( 
        k=>

         factor.set(k, 
          {

            
            def sum( i:Int):Double = {
              f( Event(n,i)::k) +
               (if ( i>0) sum(i-1) else 0.0) 
            }
            sum( n.states.length-1)
          }
        )
        )
    factor
  }

//  def sumOutOld(n: Node, f: Factor): Factor = {
//    val ff = for (i <- 0 until n.states.length) yield (reduce(Event(n, i), f))
//    doSum(ff)
//  }

  def doSum(fl: Seq[Factor]): Factor = {
    var factor = Factor(fl.head.vars, fl.head.keys)
    factor.keys.foreach(
      k => factor += (k -> fl.map(_(k)).sum))
    factor
  }
  
  def contains( f:List[List[Event]], n:Node):Boolean ={
    def contains2( l:List[Event], n:Node):Boolean ={
      if ( l.isEmpty) {
        false
      } else {
        if ( l.head.n==n) {
          true
        } else {
          contains2( l.tail,n)
        }
      }
    }
    if ( f.isEmpty) {
      false
    } else {
      if ( contains2(f.head,n)) {
        true
      } else {
        contains(f.tail,n)
      }
    }
    
  }

//  def contains(f: Factor, node: Node): Boolean = {
//    f.keys.exists(_.exists(_.n == node))
//  }

//  def reduce(e: Event, f: Factor): Factor = {
//    val sourceKeys = f.keys.filter(_.contains(e))
//    val vars = f.vars diff List(e.n)
//    val factor = new Factor(vars)
//    sourceKeys.foreach(
//      k => factor += ((k diff List(e)) -> f(k)))
//    factor
//  }

//  def product(f1: Factor, f2: Factor): Factor = {
//    val factor = new Factor(f1.vars.union(f2.vars).distinct)
//    factor.keys.foreach(k => factor.set(k,f1(k) * f2(k)))
//    factor
//  }

  implicit def seqEv2QueryCondition(l: List[Event]): QueryCondition = QueryCondition(l)
}

object Elimination2 extends InferenceAlg {
  def apply(bbn: BBN, node: Node, evidence: Event*): List[Tuple2[Symbol, Double]] = {
    val xres = eliminationAsk(bbn, node, evidence: _*)
    val r = xres.keys.map(x => (x.head, xres(x))).sortWith(
      (a, b) => a._1.state < b._1.state).map(
        a => (a._1.n.states(a._1.state), a._2))
    Util.normalize(r)
  }

  def eliminationAsk(bbn: BBN, node: Node, evidence: Event*): Factor = {
    val ev = evidence.toList
    val nodes_sorted = bbn.nodes.values.toList.sortWith(bbn.isParent(_, _))

    val hidden = Util.diff( List.empty[Node], nodes_sorted, node).diff(ev.map(_.n))
    
    
    def computeFactors( vars:List[Node], factors:List[Factor]):List[Factor] = {
      val factrs = Factor.makeFactor(vars.head, ev, bbn) :: factors
      val facts = (if ( hidden.contains( vars.head)) {
        Factor.sumOut( vars.head, factrs,bbn)
      } else {
        factrs
      })
      if ( !vars.tail.isEmpty) {
        computeFactors( vars.tail, facts)
      } else {
        facts
      }
    }
    
    Factor.product2(computeFactors( nodes_sorted, Nil))

  }


}
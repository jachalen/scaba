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
package scaba.example

import scaba._
import scaba.inference._

object Test extends App {
  test()
  
  def profile[T]( f: => T):(T,Long) = {
    val t0 = System.nanoTime()
    val r = f
    val t1 = System.nanoTime()

    (r, (t1-t0)/1000)
  }
  
  
  def meandAndSD(vs:List[Int]):Tuple4[Double,Double,Double,Int] = {
    val mean = vs.sum.toDouble/vs.length
    val sd = Math.sqrt( ((vs.map(v => (mean - v)*(mean - v))).sum)/vs.length)
    val sorted = vs.sorted
    
    (mean,sd,vs(vs.length/2),vs.length)
  }
  
  
  def test( al:InferenceAlg,  bn:BBN, query:InferenceQuery, repetitions: Int):QueryResult = {
    implicit val bbn = bn
    implicit val alg = al
    val results = for (i<-1 to repetitions) yield (
      profile { P(query) }   
    )
    val mean = results.map( _._2).sum/results.length
    val sd = Math.sqrt( ((results.map(v => (mean - v._2)*(mean - v._2))).sum)/results.length)
    (alg.getClass.getSimpleName, results.head._1, mean.toInt , sd.toInt, results.map( _._2.toInt).toList)
  }
  
  def test( al:List[InferenceAlg],  bn:BBN, query:InferenceQuery, repetitions:Int):AlgoQueryResult = {
    val results = for (alg <- al) yield (test(alg, bn, query,repetitions))
    val list =results.map(r=>(r._1,r._5)).toMap
    val equality = results.forall( _._2==results.head._2)
    val ret = (query, results, equality, list)
    println(query+"\n"+results.map(a=>(a._1,a._2,a._3,a._4)).mkString("\n"))
    ret
  } 
  
  def test2( al:List[InferenceAlg], queries: List[(BBN,InferenceQuery)], repetitions:Int):List[AlgoQueryResult] = {
    val results = queries.map( q => test(al, q._1, q._2, repetitions))
    val a = al.map( alg => (alg.getClass.getSimpleName, results.flatMap( r=>r._4(alg.getClass.getSimpleName))))
    val rrr = a.map( e=>(e._1, meandAndSD(e._2)))
    println( rrr.mkString("\n"))    
//    var aggregation = al.map(a=> (a.getClass.getSimpleName,List[Int]())).toMap
//    val r = results.tail.map( _._2.map(s=>(s._1,s._3)).toMap)
//    r.foreach( x =>
//      x.foreach( u => 
//        aggregation += (u._1 -> (u._2 :: aggregation(u._1)))
//      )
//    )
//    val uu = aggregation.map( a => {
//      val sum = a._2.sum
//      (a._1, sum/a._2.length)
//    }).toMap
//    
//    println( uu.mkString("\n"))
    
    results
  }
  
  
  def test() = {
    implicit var bbn:BBN = Alarm
    val queries = List[(BBN,InferenceQuery)]( 
        (Alarm, 'Burglary | 'MaryCalls<<'True & 'JohnCalls << 'True),
        (Alarm, 'Burglary | 'MaryCalls<<'True & 'JohnCalls << 'True),
        (Alarm, 'Burglary | 'MaryCalls<<'True ),
        (Alarm, 'Earthquake | 'MaryCalls<<'True & 'JohnCalls << 'True),
        
        (Alarm, 'Earthquake | 'MaryCalls<<'True ),
        (Alarm, 'MaryCalls | 'Earthquake<<'True),
        (Alarm, 'JohnCalls | 'Earthquake<<'True),
        (Alarm, 'MaryCalls | 'Earthquake<<'True & 'Burglary << 'False),
        (Alarm, 'JohnCalls | 'Earthquake<<'True & 'Burglary << 'False)
        )
    bbn = Student
    val queries2 = List[(BBN,InferenceQuery)](
        (Student, 'Letter | 'Intell << 'hi),
        (Student, 'Letter | 'Intell << 'lo),
        (Student, 'Letter | 'Diff << 'hi & 'SAT << 'hi),
        (Student, 'Letter | 'Diff << 'hi & 'SAT << 'lo),
        (Student, 'Letter | 'Diff << 'lo & 'SAT << 'hi),
        (Student, 'Letter | 'Diff << 'lo & 'SAT << 'lo),
        (Student, 'Intell | 'Letter << 'good & 'SAT << 'hi),
        (Student, 'Intell | 'Letter << 'bad & 'SAT << 'hi),
        (Student, 'Intell | 'Letter << 'good & 'SAT << 'lo),
        (Student, 'Intell | 'Letter << 'bad & 'SAT << 'lo)
        )
    val algs = List[InferenceAlg]( 
        Enumeration,// Elimination, 
        Elimination)
   //test2( algs, queries, 100)
    System.out.println("Starting...");
    Thread.sleep(100)
    val results:List[AlgoQueryResult]= test2( algs, queries ::: queries2, 100000) 

  }
  
  
  type QueryResult = (String, List[(Symbol,Double)],Int,Int,List[Int])
  type AlgoQueryResult = (InferenceQuery,List[QueryResult], Boolean,Map[String,List[Int]])
  

}
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
package object scaba extends ScabaAPI {


}

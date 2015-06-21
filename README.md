# scaba

A Scala Implementation for Bayesian Networks

<code>
Welcome to Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_17).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import scaba._
import scaba._
</code>
scala> implicit val bbn = example.Alarm
bbn: scaba.example.Alarm.type =
P('JohnCalls|'Alarm)
P('Alarm|'Burglary&'Earthquake)
P('MaryCalls|'Alarm)
P('Earthquake)
P('Burglary)

scala> P('Burglary | 'MaryCalls << 'True & 'JohnCalls << 'True)

res0: List[(Symbol, Double)] = List(('True,0.2841718353643929), ('False,0.7158281646356071))

scala> val test = new BBN
test: scaba.BBN =

scala> implicit val bbn = test

bbn: scaba.BBN =

scala> 'A:=('t,'f)
'B:=('t,'f)
res1: scaba.Node = 'A

scala> res2: scaba.Node = 'B

scala> P('A)++=(§ -> %(0.2,0.8))
P('B | 'A) ++= ((§('A<<'t)-> %(0.1,0.9)),(§('A<<'f)-> %(0.6,0.4)))

res3: scaba.NodeInfo =
P('A)

        't    'f
( 0) 0.200 0.800

scala> res4: scaba.NodeInfo =
P('B|'A)

     'A    't    'f
( 0) 'f 0.600 0.400
( 1) 't 0.100 0.900

scala> P('B | 'A) += (1 -> %(0.2,0.8))
res5: scaba.NodeInfo =
P('B|'A)

     'A    't    'f
( 0) 'f 0.600 0.400
( 1) 't 0.200 0.800

scala> P('A | 'B << 't)
res6: List[(Symbol, Double)] = List(('t,0.07692307692307694), ('f,0.923076923076923))
# scaba

A Scala Implementation for (Discrete) Bayesian Networks

(c) 2015 Jan Charles Lenk, jan.charles.lenk@gmail.com

Licensed under the LGPL.

Scaba is a DSL for Bayesian Networks (or Bayesian Belief Networks, BBN), written in Scala. Create BBNs by using
proper probabilistic notation from within your programs, worksheets, or the console.

Currently implements exact inference algorithms, i.e., variable enumeration and elimination.

* Run scaba/example/Test.scala for an evaluation of both inference algorithms
* See scaba/example/Alarm.scala for the definition of a BBN
* See scaba/example/worksheet.sc for usage in a Scala worksheet example
* Usage on the console:
```
Welcome to Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_17).
Type in expressions to have them evaluated.
Type :help for more information.
```

**import the scaba package**
```
scala> import scaba._
import scaba._
```


**pull the example Alarm BBN into context**
```
scala> implicit val bbn = example.Alarm
bbn: scaba.example.Alarm.type =
P('JohnCalls|'Alarm)
P('Alarm|'Burglary&'Earthquake)
P('MaryCalls|'Alarm)
P('Earthquake)
P('Burglary)
```

**Perform an inference for Burglary node**
```
scala> P('Burglary | 'MaryCalls << 'True & 'JohnCalls << 'True)

res0: List[(Symbol, Double)] = List(('True,0.2841718353643929), ('False,0.7158281646356071))
```
**Define a new BBN from scratch**
```
scala> val test = new BBN
test: scaba.BBN =
```

**pull new BBN into context**
```
scala> implicit val bbn = test

bbn: scaba.BBN =
```

**Define to new nodes**
```
scala> 'A:=('t,'f)
'B:=('t,'f)
res1: scaba.Node = 'A

scala> res2: scaba.Node = 'B
```


**Define tables and dependencies for the two nodes 'A and 'B. Nodes are defined by Scala Symbols as names.**
```
scala> P('A)++=(Nil --> (0.2,0.8))
P('B | 'A) ++= (('A<<'t) --> (0.1,0.9),('A<<'f)--> (0.6,0.4))

res3: scaba.NodeInfo =
P('A)

        't    'f
( 0) 0.200 0.800

scala> res4: scaba.NodeInfo =
P('B|'A)

     'A    't    'f
( 0) 'f 0.600 0.400
( 1) 't 0.100 0.900
```

**Change line 1 for probability table from node 'B**
```
scala> P('B | 'A) += (1 -> %(0.2,0.8))
res5: scaba.NodeInfo =
P('B|'A)

     'A    't    'f
( 0) 'f 0.600 0.400
( 1) 't 0.200 0.800
```

**Perform an inference**
```
scala> P('A | 'B << 't)
res6: List[(Symbol, Double)] = List(('t,0.07692307692307694), ('f,0.923076923076923))
```

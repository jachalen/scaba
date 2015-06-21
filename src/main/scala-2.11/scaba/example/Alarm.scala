package scaba.example

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

import scaba._


object Alarm extends BBN {
  name="Alarm"
  'Burglary := ('True, 'False) 
  'Earthquake := ('True, 'False) 
  'Alarm := ('True, 'False) 
  'MaryCalls := ('True, 'False) 
  'JohnCalls := ('True, 'False) 
  

  
  P('Burglary)
  P('Earthquake) 
  P('Alarm | 'Burglary & 'Earthquake) 
  P('JohnCalls | 'Alarm)
  P('MaryCalls | 'Alarm)

  
  Table('Burglary) ++= ( § -> %(0.001,0.999))
  Table('Earthquake) ++= (§ -> %(0.002, 0.998))
 
 Table('Alarm) ++= ((§('Burglary << 'True, 'Earthquake << 'True) -> %(0.95, 0.05)),
    (§('Burglary << 'True, 'Earthquake << 'False) -> %(0.94, 0.06)),
    (§('Burglary << 'False, 'Earthquake << 'True) -> %(0.29, 0.71)),
    (§('Burglary << 'False, 'Earthquake << 'False) -> %(0.001, 0.999)))
  
  Table('JohnCalls) ++= ((§('Alarm << 'True) -> %(0.9, 0.1)),
    (§('Alarm << 'False) -> %(0.05, 0.95)))
    

  Table('MaryCalls) ++= ((§('Alarm << 'True) -> %(0.7, 0.3)),
    (§('Alarm << 'False) -> %(0.01, 0.99)))

}
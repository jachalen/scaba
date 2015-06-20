package de.scaba.example

import de.scaba._
import de.scaba.BBN._

/**
 * Student Example, from: 
 * Koller, D.(2009): Probabilistic Graphical Models Principles and Techniques, Ch. 3.2
 * @author jan.charles.lenk@gmail.com
 *
 */
object Student extends BBN {
  name="Student"
  //Course Difficulty
  'Diff := ( 'hi, 'lo)
  //Student's intelligence
  'Intell := ('hi, 'lo)
  //Student's score in SAT test
  'SAT := ('hi,'lo)
  //Achieved grade in course
  'Grade := ('hi,'med, 'lo)
  //Quality of professor's recommendation letter
  'Letter := ('good, 'bad)
  
  P('Diff) ++= (§ -> %( 0.6, 0.4))
  P('Intell) ++= (§ -> %(0.7, 0.3))
  P('SAT | 'Intell) ++= (( §( 'Intell << 'hi) -> %(0.95,0.05)),
                           §( 'Intell << 'lo) -> %(0.2,0.8))
  P('Grade | 'Diff & 'Intell) ++= (( §('Diff << 'hi, 'Intell <<'hi)-> %(0.3,0.4,0.3)),
      ( §('Diff << 'hi, 'Intell <<'lo)-> %(0.05,0.25,0.7)),
      ( §('Diff << 'lo, 'Intell <<'hi)-> %(0.9, 0.08, 0.02)),
      ( §('Diff << 'lo, 'Intell <<'lo)-> %(0.5,0.3,0.2))
      )
  
  P('Letter | 'Grade) ++= ((§('Grade << 'hi)-> %(0.1, 0.9)),
    (§('Grade << 'med) -> %(0.4, 0.6)),
    (§('Grade << 'lo) -> %(0.99, 0.01)))
}
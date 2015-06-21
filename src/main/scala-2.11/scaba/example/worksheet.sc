import scaba._

/**
 * Demo worksheet usage
 *
 * Scaba (c) 2015 Jan Charles Lenk
 * @author jan.charles.lenk@gmail.com
 */
object AlarmWorksheet extends BBN {
  name = "Alarm"
  println("Ji")

  'B:=('t,'f)
  'E:=('t,'f)
  'A:=('t,'f)
  'J:=('t,'f)
  'M:=('t,'f)


  P( 'B)
  P( 'E)

  P( 'A | 'B & 'E)
  P( 'J | 'A)
  P( 'M | 'A)


  Table('B) ++= {§ -> %(0.001,0.999)}


  Table('E) ++= {§ -> %(0.002,0.998)}

  Table('A)++= (
    §('B<<'t,'E<<'t) -> %(0.95,0.05),
    §('B<<'t,'E<<'f) -> %(0.94,0.06),
    §('B<<'f,'E<<'t) -> %(0.29,0.71),
    §('B<<'f,'E<<'f) -> %(0.001,0.999))

  Table( 'J)++= (
     §('A<<'t) -> %(0.9,0.1),
     §('A<<'f) -> %(0.05,0.95))

  Table( 'M)++= (
    §('A<<'t) -> %(0.7,0.3),
    §('A<<'f) -> %(0.01,0.99) )
}

implicit val bbn = AlarmWorksheet
P( 'B | 'J<<'t & 'M << 't)
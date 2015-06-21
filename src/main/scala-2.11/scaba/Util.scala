package scaba

object Util {
  def normalize(l: List[Tuple2[Symbol, Double]]): List[Tuple2[Symbol, Double]] = {
    val sum = l.map( _._2).sum
    //l.foreach(sum += _._2)
    l.map(a => (a._1, a._2 / sum))
  }
  
  def diff[T]( previous:List[T], list:List[T], elem:T):List[T] = {
    if (list.head==elem) {
      previous:::list.tail
    } else {
      if ( list.tail.isEmpty) {
        list
      } else {
        diff( list.head::previous, list.tail, elem)
      }
    }
    
    
  }
  
//  
//  def loop[T](ls:List[T])(f:T=>Unit):Unit ={
//    if ( !ls.isEmpty) {
//      f(ls.head)
//      loop( ls.tail)(f)
//    }
//  }


}
/**
 * Created by leonardo on 13/03/15.
 */
package object HighOrder {
  def merge[T](ints: List[T], ints1: List[T], f: (T,T) => T) = {
        ints.zip(ints1).map({a:(T,T) => f(a._1, a._2)})
  }
  def merge2[T](ints: List[T], ints1: List[T])(implicit f: (T,T) => T) ={
    ints.zip(ints1).map({a:(T,T) => f(a._1, a._2)})
  }


}

package loom

class Loom(tieUp: Pattern, threading: Pattern) {
  private val threads = threading.list
  val tieup = tieUp.list
  val numheddles = tieup(0)
  val numpedals = tieup(1)
  val hpp = tieup(2)
  private val heddles = List.range(0,numheddles).map(i => threads.map(e => e==(i+1)))
  private def orcomb(A: List[Boolean], B: List[Boolean]): List[Boolean] = {
    A match {
      case List() => List()
      case x :: xA => B match {
        case List() => List()
        case y :: xB => (x || y) :: orcomb(xA,xB) 
      }
    }
  }
  private def combinepedal(p: List[Int]): List[Boolean] = {
    def combl(l: List[Int]): List[Boolean] = {
      l match {
        case Nil => List()
        case x :: Nil => heddles(x-1) 
        case x :: xl => orcomb(heddles(x-1), combl(xl))
      }
    }
    combl(p)
  }
  
  
  private val pedals = List.range(0,numpedals).map(i => combinepedal(tieup.slice(3+i*hpp,3+(i+1)*hpp)))
  val width = threads.size
  
  def weave(pedalling: Pattern): List[List[Boolean]] = {
    pedalling.list.map(p => pedals(p-1))
  }
  
}
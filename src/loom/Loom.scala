package loom


// the actual digital loom
//
// it takes two patterns to set up:
// - tieUp should contain v1) num heddles, v2) num pedals, v3) num heddles per pedals
// and then v2 x v3 numbers indicating the heddles tied per pedal 
// - threading contains the heddle to which each warp thread is connected

class Loom(tieUp: Pattern, threading: Pattern) {
  private val threads = threading.list
  private val width = threads.size
  private val tieup = tieUp.list
  private val numheddles = tieup(0)
  private val numpedals = tieup(1)
  private val hpp = tieup(2)
  
  // heddles contains for each heddle per warp thread if it is connected to that heddle
  private lazy val heddles = List.range(0,numheddles).map(i => threads.map(e => e==(i+1)))
  
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
  
  // pedals contains for each pedal the crossing of all waft threads
  private lazy val pedals = List.range(0,numpedals).map(i => combinepedal(tieup.slice(3+i*hpp,3+(i+1)*hpp)))
  
  // the main routine: actual weaving
  //
  // takes a pattern that shows the order of pedalling and
  // returns a list of lists of boolean that indicates for each
  // crossing of warp and weft threads if warp is on top or below
  
  def weave(pedalling: Pattern): List[List[Boolean]] = {
    pedalling.list.map(p => pedals(p-1))
  }
  
}
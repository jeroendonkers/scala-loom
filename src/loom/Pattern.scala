package loom
import scala.collection.mutable.Map 

// patterns are basically lists of integers
// these classes allow to build patterns out of other patterns
// using operators +, -, * and named patterns.

sealed case class Nlist(n: List[Int]) extends Pattern
sealed case class Plist(p: List[Pattern]) extends Pattern
sealed case class Ptimes(t: Int, p: Pattern) extends Pattern

abstract class Pattern {
   def toList(pat: Pattern): List[Int] = {
     pat match {
     case Nlist(n: List[Int]) =>
       n
     case Plist(p: List[Pattern]) =>
       p.map(toList).flatten
     case Ptimes(t: Int, p: Pattern) =>
       List.fill(t)(toList(p)).flatten
   } 
  }
   def list: List[Int] = toList(this) 
   
   def mkString(pat: Pattern): String = {
     pat match {
     case Nlist(n: List[Int]) =>
       n.mkString("[",",","]")
     case Plist(p: List[Pattern]) =>
       p.map(mkString).mkString("(","+",")")
     case Ptimes(t: Int, p: Pattern) =>
       ""+mkString(p)+"x"+t
   }
 }
 override def toString: String = {
    mkString(this) 
 }  
  
 def reverse(pat: Pattern): Pattern = {
     pat match {
     case Nlist(n: List[Int]) =>
       Nlist(n.reverse)      
     case Plist(p: List[Pattern]) =>
       Plist(p.map(reverse))
     case Ptimes(t: Int, p: Pattern) =>
       Ptimes(t,reverse(p))
   }
 }
 def reverse: Pattern = reverse(this)
 
 def size(pat: Pattern): Int = {
     pat match {
     case Nlist(n: List[Int]) =>
       n.size      
     case Plist(p: List[Pattern]) =>
       (p.map(size) foldLeft 0) {(x, y) => x + y}
     case Ptimes(t: Int, p: Pattern) =>
       t * size(p)
   }
 }
 def size: Int = size(this)
 
 
 def +(that: Pattern) = Plist(List(this,that))
 def +(that: List[Int]) = Plist(List(this,Nlist(that)))
 def -(that: Pattern) = Plist(List(this,reverse(that)))
 def -(that: List[Int]) = Plist(List(this,Nlist(that.reverse)))
 def *(that: Int) = Ptimes(that,this)
 def unary_- = reverse(this)
}


object Pattern { 
  
 // factory functions  
  
 def apply(l: List[Int]) = Nlist(l)
 def apply() = Nlist(List())
 def apply(a: Int) = Nlist(List(a))
 def apply(a: Int, b: Int) = Nlist(List(a,b))
 def apply(a: Int, b: Int, c: Int) = Nlist(List(a,b,c))
 def apply(a: Int, b: Int, c: Int, d: Int) = Nlist(List(a,b,c,d))
 def apply(a: Int, b: Int, c: Int, d: Int, e: Int) = Nlist(List(a,b,c,d,e))
 def apply(l: List[Pattern]) = Plist(l)
 def apply(s: String) = {
   if (patmap.isDefinedAt(s.toLowerCase)) patmap(s.toLowerCase) else null
 }
 
 // patmap stores named patterns. Pattern names are stored lowercase.
 
 private val patmap:Map[String,Pattern] = Map()
 
 def define(s: String, pat: Pattern) = {
   if (isdefined(s)) patmap(s.toLowerCase) = pat 
   else patmap += (s.toLowerCase -> pat)
 }
 def isdefined(s: String) = {
   patmap.isDefinedAt(s.toLowerCase)
 }
 def clearmap() = { 
    patmap.clear()
 }
 
 def getPatterns: Iterator[String] = {
   patmap.keys.iterator
 }
 
 
 clearmap()
}

  
  
   

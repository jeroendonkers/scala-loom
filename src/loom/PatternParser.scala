package loom
import scala.util.parsing.combinator._
import scala.collection.mutable.SortedSet
import scala.io.Source
import java.io._

// parses/interpret a string and store all patterns as named patterns

object PatternParser extends JavaTokenParsers {
   
  // set to collect errors during parsing
   val errors: SortedSet[String] = SortedSet()
   
   override def failure(msg: String) = { errors.add(msg); "" ~> super.failure("") }
   
   // load the contents of a library. At the moment, 
   // libraries must reside under a directory "patterns" and must be named
   // ".lib".
    def load: Parser[String] = ">" ~ ident ^^ {
      case ">" ~ f => {
      if (Pattern.isdefined(f)) {
         errors.add("E02 lib "+f+" already loaded")
         ""
      } else {  
         val lib = new File("patterns/"+f+".lib") 
         if (!lib.exists()) {
           errors.add("E02 lib "+f+" not found") 
           ""
         } else {
           val text = Source.fromFile(lib).mkString
           val result = this.eval(text,false,false)
           f + " loaded"
         }
        }  
      }
   } 
   
    def integer : Parser[Int] =  wholeNumber ^^ {
        case x => x.toInt
    } 
    
    def intlist : Parser[Pattern] = "["~>repsep(integer,",")<~"]" ^^ {
      case ints => Pattern(ints)
    }
    
    def expr : Parser[Pattern] = chainl1(factor, 
        "+" ^^^ {(a: Pattern, b: Pattern) => a + b} |    
        "-" ^^^ {(a: Pattern, b: Pattern) => a - b}) 
      
    def negfactor  : Parser[Pattern] =  "-"~factor ^^ { case "-"~pat => -pat}
      
    def bracketlist: Parser[Pattern] = "("~>expr<~")" ^^ {
      case pat => pat
    }
    
    def mulfactor : Parser[Pattern] =  
      integer~"x"~listfactor^^{ case i~"x"~p => Ptimes(i,p)} |
      listfactor~"x"~integer^^{ case p~"x"~i => Ptimes(i,p)} |
      integer~"*"~listfactor^^{ case i~"*"~p => Ptimes(i,p)} |
      listfactor~"*"~integer^^{ case p~"*"~i => Ptimes(i,p)} 
        
    def listfactor : Parser[Pattern] = intlist | bracketlist | idfactor
    
    def idfactor : Parser[Pattern] = ident ^^ { 
      case id => 
        if (Pattern.isdefined(id)) Pattern(id) 
        else {
          errors.add("E01 Pattern \""+id + "\" not defined.")
          Pattern()
        }  
    }
    
    def factor : Parser[Pattern] = negfactor | mulfactor | listfactor    
    
    def patdef: Parser[String] = ident~"="~expr ^^ {
      case id ~ "=" ~expr => { if (!Pattern.isdefined(id)) { Pattern.define(id,expr)}; id }
    }  
    
    def statement: Parser[String] = patdef | load 
    
    def script: Parser[List[String]] = repsep(statement,";") <~ ";" | repsep(statement,";")
    
    
    // main function
    
    def eval(input:String, complete: Boolean = true, clear: Boolean =true): Option[List[String]] = {
      
      if (clear) {
        errors.clear
        Pattern.clearmap()
     }    
      
      // remove comments: "#"
      val newinput =  input.split("\n").map(_.split("#")(0)).mkString("\n")
      
      parseAll(script, newinput) match {
        case Success(result, _) => {
          if (complete) checkPatterns
          if (errors.size>0) None
          else Some(result)
        }
        case _ => {
          errors.add("E00 Syntax error.")
          None
        }
      }
    }
    
    
    // check all patterns on semantic level for our loom
    
    def checkPatterns: Unit = {
        Pattern.getPatterns.foreach(s => Pattern(s).list.foreach(k=>if (k<0) {errors.add("E04 Incorrect number in "+s+"."); return} ))
      
        // check tieup
        if (!Pattern.isdefined("tieup")) { errors.add("E03 Pattern \"tieup\" is missing."); return }
        val tieup = Pattern("tieup").list
        tieup.foreach(k => if (k<=0) { errors.add("E04 Incorrect number in tieup."); return })
        if (tieup.size<1) { errors.add("E04 Number of heddles is missing in tieup."); return }
        if (tieup.size<2) { errors.add("E04 Number of pedals is missing in tieup."); return }
        if (tieup.size<3) { errors.add("E04 Number of heddles per pedals is missing in tieup."); return }
        val numheddles = tieup(0)
        val numpedals = tieup(1)
        val ppp = tieup(2)
        if (ppp>numheddles) { errors.add("E04 Number of heddles per pedals larger than number of heddles in tieup."); return }
        if (tieup.size != numpedals*ppp+3) { errors.add("E04 Incorrect length of tieup."); return }
        tieup.drop(3).foreach(k => if (k>numheddles) { errors.add("E04 Incorrect heddle number in tieup."); return })

        if (!Pattern.isdefined("threading")) { errors.add("E03 Pattern \"threading\" is missing."); return }
        Pattern("threading").list.foreach(k => if (k<=0 | k>numheddles) { errors.add("E04 Incorrect number in threading."); return })
        
        if (!Pattern.isdefined("pedalling")) errors.add("E03 Pattern \"pedalling\" is missing.")
        Pattern("pedalling").list.foreach(k => if (k<=0 | k>numpedals) { errors.add("E04 Incorrect number in pedalling."); return })
        
        if (!Pattern.isdefined("warpcolors")) errors.add("E03 Pattern \"warpcolors\" is missing.")
        Pattern("warpcolors").list.foreach(k => if (k<=0) { errors.add("E04 Incorrect number in warpcolors."); return })
        
        if (!Pattern.isdefined("weftcolors")) errors.add("E03 Pattern \"weftcolors\" is missing.")
        Pattern("weftcolors").list.foreach(k => if (k<=0) { errors.add("E04 Incorrect number in weftcolors."); return })

        List.range(0,9).foreach(k => if (Pattern.isdefined("color"+(k+1))) {
          val col = Pattern("color"+(k+1)).list
          if (col.size != 3) { errors.add("E04 Color"+(k+1)+" should contain 3 numbers."); return }
          col.foreach(i => if (i<0 | i>255)  { errors.add("E04 Incorrect number in color"+(k+1)+"."); return })
        } )
      }  
    }      
        
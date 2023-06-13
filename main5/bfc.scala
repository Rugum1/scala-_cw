// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {


// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example pgrams ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?
//Init tests

// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]
val alphabeticalRange= 'A' to 'Z' 

import io.Source
import scala.util._


// TASKS
//=======

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-pgram. This function takes a bf-pgram 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information about
//     pc positions where '[' or a ']' are stored. The information
//     is to which pc-position do we need to jump next?
// 
//     For example for the pgram
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 
//     in order to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should immediately look up the jump address in the jtable.

def load_bff(name: String) : String = 
    {
        Try(Source.fromFile("./" ++ name).getLines.toList.mkString(" ")).getOrElse("");
    }

 def sread(mem: Mem, mp: Int) : Int = Try(mem(mp)).getOrElse(0)

 def write(mem: Mem, mp: Int, v: Int) : Mem = Try(mem + (mp -> v)).getOrElse(mem)

 def jumpRight(pg: String, pc: Int, level: Int) : Int = pg.charAt(pc) match {
        
        case _ if(pc+1 == pg.length) => pc + 1
        case '[' => jumpRight(pg,pc+1,level+1)
        case ']' => if(level  ==  0)  pc + 1 else jumpRight(pg,pc+1,level-1)
        case _ => jumpRight(pg,pc+1,level)
    }

    def jumpLeft(pg: String, pc: Int, level: Int) : Int = pg.charAt(pc) match 
    {            
        case _ if(pc == 0 && level == 1) => pc - 1
        case ']' => jumpLeft(pg,pc-1,level+1)
        case '[' => if(level == 0) pc + 1 else jumpLeft(pg,pc-1,level-1)
        case _   => jumpLeft(pg,pc-1,level)
    }
 

def jtable(pg: String) : Map[Int, Int] = 
{ 
 
    val listOfString = pg.toList.zipWithIndex.filter(elements => elements._1 == '[' || elements._1 == ']')

    listOfString.map{ case element if(element._1 == '[') => (element._2,jumpRight(pg,element._2 + 1,0))
                      case element if(element._1 == ']') => (element._2,jumpLeft(pg,element._2 - 1,0))}.toMap 
   
}


// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = pg match 
{
  case pg  => if(Try(pg.charAt(pc)).getOrElse(0) == 0) mem 
                     else pg.charAt(pc) match {
                                    case'>'=>compute2(pg,tb,pc + 1, mp + 1,mem)
                                    case'<'=>compute2(pg,tb,pc + 1, mp - 1,mem)
                                    case'+'=>compute2(pg,tb,pc + 1, mp, write(mem,mp,sread(mem,mp) + 1))
                                    case'-'=>compute2(pg,tb,pc + 1, mp, write(mem,mp,sread(mem,mp) - 1))
                                    case'.'=>print(sread(mem,mp).toChar); 
                                             compute2(pg,tb,pc + 1,mp,mem)
                                    case'['=>if(sread(mem,mp)==0) 
                                    compute2(pg,tb,tb(pc), mp, mem) else compute2(pg,tb,pc + 1,mp,mem)
                                    case']'=>if(sread(mem,mp)!=0) compute2(pg,tb,tb(pc),mp,mem) else compute2(pg,tb,pc + 1,mp,mem)
                                    case _ => compute2(pg,tb,pc + 1,mp, mem)
                                    
                                    }
}
def run2(pg: String, m: Mem = Map()) = compute2(pg,jtable(pg),0,0,m)


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = 
{
  val reg1 = """[^<>+\-.\[\]]""".r
  val reg2 =  """\[-\]""".r
  val s2 = reg1.replaceAllIn(s,"_").filter(element => element != '_')
  reg2.replaceAllIn(s2,"0") 
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem =  pg match 
{
  case pg  => if(Try(pg.charAt(pc)).getOrElse(0) == 0) mem 
                     else pg.charAt(pc) match {
                                    case'>'=>compute3(pg,tb,pc + 1, mp + 1,mem)
                                    case'<'=>compute3(pg,tb,pc + 1, mp - 1,mem)
                                    case'+'=>compute3(pg,tb,pc + 1, mp, write(mem,mp,sread(mem,mp) + 1))
                                    case'-'=>compute3(pg,tb,pc + 1, mp, write(mem,mp,sread(mem,mp) - 1))
                                    case'.'=>print(sread(mem,mp).toChar); 
                                             compute3(pg,tb,pc + 1,mp,mem)
                                    case'['=>if(sread(mem,mp)==0) 
                                    compute3(pg,tb,tb(pc), mp, mem) else compute3(pg,tb,pc + 1,mp,mem)
                                    case']'=>if(sread(mem,mp)!=0) compute3(pg,tb,tb(pc),mp,mem) else compute3(pg,tb,pc + 1,mp,mem)
                                    case '0' => compute3(pg,tb,pc + 1,mp,write(mem,mp,0))
                                    case _ => compute3(pg,tb,pc + 1,mp, mem)
                                    
                                    }
}

def run3(pg: String, m: Mem = Map()) = compute3(pg,jtable(pg),0,0,m)


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.


def getCombinedTable(s: String,pc : Int, first :Int, map : Map[Int,Int] ): Mem = s match 
{
  case s => if(Try(s.charAt(pc)).getOrElse(0) == 0) map 
            else s.charAt(pc) match {
                     
                     case character if((pc - first + 1) == 26 && first != -1) =>  getCombinedTable(s,pc+1,-1,write(map,first, pc-first+1))
                     case '0' => getCombinedTable(s,pc+1,first,map)
                     case ']' => getCombinedTable(s,pc+1,first,map)
                     case '[' => getCombinedTable(s,pc+1,first,map)
                     case '.' => getCombinedTable(s,pc+1,first,map)
                     case character if(Try(s.charAt(pc+1)).getOrElse(0) == character) => 
                     if(first == -1)  getCombinedTable(s,pc+1,pc,map) else getCombinedTable(s,pc+1,first,map)
                     case character if(Try(s.charAt(pc+1)).getOrElse(0) != character) => 
                     if(Try(s.charAt(pc-1)).getOrElse(0) == character && first != -1)
                     getCombinedTable(s,pc+1,-1,write(map,first, pc-first+1))
                     else 
                     getCombinedTable(s,pc+1,-1,write(map,pc,1))
            }
}


def createCombinedString(s:String, s2: String, pc : Int, map: Map[Int,Int]): String = s match 
{
  case s => if(Try(s.charAt(pc)).getOrElse(0) == 0) s2
            else s.charAt(pc) match { 
              case '[' => createCombinedString(s,s2+'[',pc+1,map)
              case ']' => createCombinedString(s,s2+']',pc+1,map)
              case '.' => createCombinedString(s,s2+'.',pc+1,map)
              case '0' => createCombinedString(s,s2+'0',pc+1,map)
              case  character  => {
              val alphabeticalRange = 'A' to 'Z'  
              createCombinedString(s, s2 + (character + alphabeticalRange.lift(map(pc) - 1).get.toString) ,pc + map(pc),map)
              }
                    
            }
           
}

def combine(s: String) : String = createCombinedString(s,"",0,getCombinedTable(s,0,-1,Map()))


// testcaseoptimise(load_bff("benchmark.bf")).length == 181
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = pg match 
{
  case pg  => if(Try(pg.charAt(pc)).getOrElse(0) == 0) mem 
  else pg.charAt(pc) match {

    case'>'=>compute4(pg,tb,pc + 2, mp + alphabeticalRange.indexOf(pg(pc + 1)) + 1,mem)
    case'<'=>compute4(pg,tb,pc + 2, mp - alphabeticalRange.indexOf(pg(pc + 1)) - 1,mem)
    case'+'=>compute4(pg,tb,pc + 2, mp, write(mem,mp,sread(mem,mp) + alphabeticalRange.indexOf(pg(pc + 1))+1))
    case'-'=>compute4(pg,tb,pc + 2, mp, write(mem,mp,sread(mem,mp) - alphabeticalRange.indexOf(pg(pc + 1))-1))
    case'.'=>print(sread(mem,mp).toChar); 
             compute4(pg,tb,pc + 1,mp,mem)
    case'['=>if(sread(mem,mp)==0) 
    compute4(pg,tb,tb(pc), mp, mem) else compute4(pg,tb,pc + 1,mp,mem)
    case']'=>if(sread(mem,mp)!=0) compute4(pg,tb,tb(pc),mp,mem) else compute4(pg,tb,pc + 1,mp,mem)
    case '0' => compute4(pg,tb,pc + 1,mp,write(mem,mp,0))
    case _ => compute4(pg,tb,pc + 1,mp, mem)

  }

}


// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = 
{
 val combinedString = combine(optimise(pg))
 val jTable = jtable(combinedString) 
  compute4(combinedString,jTable,0,0,m)
}

// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}

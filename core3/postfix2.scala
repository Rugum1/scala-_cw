  // Shunting Yard Algorithm 
  // including Associativity for Operators 
  // =====================================

  object C3b {


  // type of tokens
  type Toks = List[String]

  // helper function for splitting strings into tokens
  def split(s: String) : Toks = s.split(" ").toList

  // left- and right-associativity
  abstract class Assoc
  case object LA extends Assoc
  case object RA extends Assoc


  // power is right-associative,
  // everything else is left-associative
  def assoc(s: String) : Assoc = s match {
    case "^" => RA
    case _ => LA
  }
  
   val ops = List("+", "-", "*", "/", "^")

  // the precedences of the operators
  val precs = Map("+" -> 1,
        "-" -> 1,
      "*" -> 2,
      "/" -> 2,
                  "^" -> 4)

  val parentheses = Map(")" -> "(",
		"]" -> "[",
		"}" -> "{")

 // the left parentheses used in the algorithm
 val left_parentheses = List("(","[","{")

 // the right parentheses used in the algorithm
 val right_parentheses = List(")","]","}")
 

  def is_op(op: String) : Boolean = ops.contains(op)

  def precLeftAssoc(op1: String, op2: String) : Boolean = {
    
    if ( precs.get(op1).get  > precs.get(op2).getOrElse(0) ) 
    {
       false 
    }
    else 
    {
      
      true 
    
    }

  }

  def precRightAssoc(op1: String, op2: String) : Boolean = precs.get(op1).get <  precs.get(op2).getOrElse(0)

  def isAllDigits(x: String) = x forall Character.isDigit 

  def get_popped_elements(st: Toks,left_parentheses : String) : Toks = st.dropRight(st.length - st.indexOf(left_parentheses))

  def get_head(st: Toks) : String = st match {

	case Nil => ""
	case element :: rest => element
}

def pop_elements(st: Toks,left_parentheses : String) : Toks = st match {

	case Nil => Nil
	case element :: rest if (left_parentheses == element) => rest 
	case element :: rest => pop_elements(rest,left_parentheses) 
}

def pop_symbols_from_stack(st: Toks, symbol : String) : Toks = st match 
{
   case Nil => st 
   case element :: rest if(assoc(symbol) == RA && precRightAssoc(symbol,element)) => pop_symbols_from_stack(rest,symbol)
   case element :: rest if(assoc(symbol) == RA && precRightAssoc(symbol,element) == false) => st 
   case element :: rest if(assoc(symbol) == LA && precLeftAssoc(symbol,element)) => pop_symbols_from_stack(rest,symbol)
   case element :: rest if(assoc(symbol) == LA && precLeftAssoc(symbol,element) == false) => st 
}

def get_symbols_popped_from_stack(st: Toks , symbol : String,symbols_popped : Toks = Nil ) : Toks = st match 
{
   case Nil => symbols_popped
   case element :: rest if(assoc(symbol) == RA && precRightAssoc(symbol,element)) => get_symbols_popped_from_stack(rest,symbol, symbols_popped ::: element :: Nil)
   case element :: rest if(assoc(symbol) == RA && precRightAssoc(symbol,element) == false) => symbols_popped
   case element :: rest if(assoc(symbol) == LA && precLeftAssoc(symbol,element)) =>  get_symbols_popped_from_stack(rest,symbol, symbols_popped ::: element :: Nil)
   case element :: rest if(assoc(symbol) == LA && precLeftAssoc(symbol,element) == false) => symbols_popped
}

  def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
  
   
	case Nil => out ::: st 
	case element :: rest if isAllDigits(element) => syard(rest,st,out ::: element :: Nil)
  case element :: rest if (is_op(element) && assoc(element) == RA && precRightAssoc(element,get_head(st))) => syard(rest, element :: pop_symbols_from_stack(st,element)  ,out ::: get_symbols_popped_from_stack(st,element,List()))
  case element :: rest if (is_op(element) && assoc(element) == RA && precRightAssoc(element,get_head(st)) == false ) => syard(rest,element :: st, out)
  case element :: rest if (is_op(element) && assoc(element) == LA && precLeftAssoc(element,get_head(st))) => syard(rest, element :: pop_symbols_from_stack(st,element) ,out ::: get_symbols_popped_from_stack(st,element,List()))
  case element :: rest if (is_op(element) && assoc(element) == LA && precLeftAssoc(element,get_head(st)) == false ) => syard(rest,element :: st, out)
	case element :: rest if (left_parentheses.contains(element)) => syard(rest, element :: st , out)
	case element :: rest if (right_parentheses.contains(element)) => syard(rest, pop_elements(st,parentheses.get(element).get),out ::: get_popped_elements(st,parentheses.get(element).get)  )
	  
}

  // (3) Implement the extended version of the shunting yard algorithm.
  // This version should properly account for the fact that the power 
  // operation is right-associative. Apart from the extension to include
  // the power operation, you can make the same assumptions as in 
  // basic version.

  


  // test cases
  // C3b.syard(C3b.split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


  // (4) Implement a compute function that produces an Int for an
  // input list of tokens in postfix notation.

 def calculate(firstElement : Int, secondElement : Int, symbol : String): Int =  symbol match 
{
	case "+" => firstElement + secondElement
	case "-" => firstElement - secondElement
	case "*" => firstElement * secondElement
	case "/" => firstElement / secondElement
  case "^" =>  BigInt(firstElement).pow(secondElement).toInt
}


def pop_two_elements(st: List[Int], symbol : String) : Int = st match 
{
  case Nil => 0
	case firstElement :: secondElement :: Nil => calculate(firstElement, secondElement,symbol)
	case firstElement :: secondElement :: rest => pop_two_elements(secondElement :: rest, symbol)
}

  def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match 
  {
     
  case Nil => st.head 
	case element :: rest if isAllDigits(element) => compute(rest,st ::: element.toInt :: Nil)
	case element :: rest if is_op(element) => compute(rest, st.dropRight(2) ::: pop_two_elements(st,element):: Nil)

  }


  // test cases
  // C3b.compute(C3b.syard(C3b.split("3 + 4 * ( 2 - 1 )")))   // 7
  // C3b.compute(C3b.syard(C3b.split("10 + 12 * 33")))       // 406
  // C3b.compute(C3b.syard(C3b.split("( 5 + 7 ) * 2")))      // 24
  // C3b.compute(C3b.syard(C3b.split("5 + 7 / 2")))          // 8
  // C3b.compute(C3b.syard(C3b.split("5 * 7 / 2")))          // 17
  // C3b.compute(C3b.syard(C3b.split("9 + 24 / ( 7 - 3 )"))) // 15
  // C3b.compute(C3b.syard(C3b.split("4 ^ 3 ^ 2")))      // 262144
  // C3b.compute(C3b.syard(C3b.split("4 ^ ( 3 ^ 2 )")))  // 262144
  // C3b.compute(C3b.syard(C3b.split("( 4 ^ 3 ) ^ 2")))  // 4096
  // C3b.compute(C3b.syard(C3b.split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

  }

// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.
//Init tests

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = 
{
  path.contains(x) == false && x._1 < dim && x._2 < dim && x._1 >=0 && x._2 >=0 
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = 
{
    val x_positions = x._1 + 1 :: x._1 + 2 :: x._1 + 2 :: x._1 + 1 :: x._1 - 1 :: x._1 -2 :: x._1 -2 :: x._1 - 1 :: Nil 
    val y_positions = x._2 + 2 :: x._2 + 1 :: x._2 - 1 :: x._2 -2 :: x._2 -2 :: x._2 -1 :: x._2 + 1  :: x._2 + 2 :: Nil 

    val all_possible_moves =  x_positions zip y_positions

    
    all_possible_moves.filter(position => is_legal(dim,path,position))

}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = 
{
    val moves = legal_moves(dim,path,x)

    moves.map(element => (element, legal_moves(dim,path,element).length))
         .sortBy(_._2)
         .map(element => element._1)
}




//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] =  xs match 
{
  case Nil => None
  case element::xs => {
      val elementValue = f(element)
      if(elementValue != None) elementValue else first(xs,f)
		
	}
}


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = path match 
{
     case elements if(path.length == dim * dim && legal_moves(dim, path.last :: Nil ,path.last).contains(path.head)) => Some(path)
     case elements => first(ordered_moves(dim,path,path.head), (position:Pos)=>first_closed_tour_heuristics(dim,position::path))
    
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] =  path match 
{
     case elements if(path.length == dim * dim ) => Some(path)
     case elements => first(ordered_moves(dim,path,path.head), (position:Pos)=>first_tour_heuristics(dim,position::path))
}



}

// Finding a single tour on a "mega" board
//=========================================

object M4c {



// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.
// Init tests



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




def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = 
{
    if(path.length == dim * dim) 
    {
        Some(path)
    }
    else 
    {   val position = path.head
        val moves = ordered_moves(dim,path,position) 
        tour_on_mega_board(dim, moves.head :: Nil ::: path)
    }
}

}

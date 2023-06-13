// Main Part 2 about Movie Recommendations 
// at Danube.co.uk
//===========================================

object M2 {

import io.Source
import scala.util._

// (1) Implement the function get_csv_url which takes an url-string
//     as argument and requests the corresponding file. The two urls
//     of interest are ratings_url and movies_url, which correspond 
//     to CSV-files.
//
//     The function should ReTurn the CSV-file appropriately broken
//     up into lines, and the first line should be dropped (that is without
//     the header of the CSV-file). The result is a list of strings (lines
//     in the file).
// Init tests
 


def get_csv_url(url: String) : List[String] = Try(Source.fromURL(url).getLines.toList.drop(1)).getOrElse(List())



val ratings_url = """https://nms.kcl.ac.uk/christian.urban/ratings.csv"""
val movies_url = """https://nms.kcl.ac.uk/christian.urban/movies.csv"""

// testcases
//-----------
//:
// val movies = get_csv_url(movies_url)
// val ratings = get_csv_url(ratings_url)

// ratings.length  // 87313
// movies.length   // 9742



// (2) Implement two functions that process the CSV-files from (1). The ratings
//     function filters out all ratings below 4 and ReTurns a list of 
//     (userID, movieID) pairs. The movies function just ReTurns a list 
//     of (movieID, title) pairs. Note the input to these functions, that is
//     the argument lines, will be the output of the function get_csv_url.


def process_ratings(lines: List[String]) : List[(String, String)] = 
{
    lines.filter( element => element.split(',').toList.lift(2).get.toInt >= 4)
         .map(element => (element.split(',').toList.lift(0).get, element.split(',').toList.lift(1).get))
}

def process_movies(lines: List[String]) : List[(String, String)] =
{
    lines.map(element => (element.split(',').toList.lift(0).get, element.split(',').toList.lift(1).get))
}


// testcases
//-----------
//val good_ratings = process_ratings(ratings)
//val movie_names = process_movies(movies)

//good_ratings.length   //48580
//movie_names.length    // 9742




// (3) Implement a grouping function that calculates a Map
//     containing the userIDs and all the corresponding recommendations 
//     (list of movieIDs). This  should be implemented in a tail
//     recursive fashion, using a Map m as accumulator. This Map m
//     is set to Map() at the beginning of the calculation.

def groupById(ratings: List[(String, String)], 
              m: Map[String, List[String]]) : Map[String, List[String]] = ratings match 
              {
                  case Nil => m 
                  case element :: tail => groupById(tail, (m + (element._1 ->  (element._2 :: m.getOrElse(element._1, List())))))
              }


// testcases
//-----------
//val ratings_map = groupById(good_ratings, Map())
//val movies_map = movie_names.toMap

//ratings_map.get("414").get.map(movies_map.get(_)) 
//    => most prolific recommender with 1227 positive ratings

//ratings_map.get("474").get.map(movies_map.get(_)) 
//    => second-most prolific recommender with 787 positive ratings

//ratings_map.get("214").get.map(movies_map.get(_)) 
//    => least prolific recommender with only 1 positive rating



// (4) Implement a function that takes a ratings map and a movie_name as argument.
//     The function calculates all suggestions containing
//     the movie in its recommendations. It ReTurns a list of all these
//     recommendations (each of them is a list and needs to have the movie deleted, 
//     otherwise it might happen we recommend the same movie).


def favourites(m: Map[String, List[String]], mov: String) : List[List[String]] =
{
    m.filter(map_element => map_element._2.contains(mov))
     .map(map_element => map_element._2.withFilter(list_element => list_element != mov)
     .map(list_element => list_element)).toList
}


// testcases
//-----------
// movie ID "912" -> Casablanca (1942)
//          "858" -> Godfather
//          "260" -> Star Wars: Episode IV - A New Hope (1977)

//favourites(ratings_map, "912").length  // => 80

// That means there are 80 users that recommend the movie with ID 912.
// Of these 80  users, 55 gave a good rating to movie 858 and
// 52 a good rating to movies 260, 318, 593.



// (5) Implement a suggestions function which takes a rating
//     map and a movie_name as arguments. It calculates all the recommended
//     movies sorted according to the most frequently suggested movie(s) first.

def suggestions(recs: Map[String, List[String]], 
                mov_name: String) : List[String] = 
                {
                 val listOfAllMovies =  recs.filter( element => element._2.contains(mov_name))
                                            .map(element => element._2).flatten



                 listOfAllMovies.groupBy(element => element).map(element => (element._1, element._2.toList.length)).toList 
                                .sortBy(_._1.toInt)
                                .sortBy(_._2)(Ordering[Int].reverse)
                                .filter(element => element._1 != mov_name)
                                .map(element => element._1)  
                        
                }

// testcases
//-----------

//suggestions(ratings_map, "912")
//suggestions(ratings_map, "912").length  
// => 4110 suggestions with List(858, 260, 318, 593, ...)
//    being the most frequently suggested movies



// (6) Implement a recommendations function which generates at most
//     *two* of the most frequently suggested movies. It ReTurns the 
//     actual movie names, not the movieIDs.


def recommendations(recs: Map[String, List[String]],
                    movs: Map[String, String],
                    mov_name: String) : List[String] = 
                    {
                        val movies = suggestions(recs,mov_name)

                        if(movies.length == 0)
                        {
                            List() 
                        } 
                        else 
                        {
                            if(movies.length == 1)
                            {
                                List(movs(movies.lift(0).get)) 
                            }
                            else 
                            {
                                List(movs(movies.lift(0).get),movs(movies.lift(1).get))
                            }
                        }
                    }



// testcases
//-----------
// recommendations(ratings_map, movies_map, "912")
//   => List(Godfather, Star Wars: Episode IV - A NewHope (1977))

//recommendations(ratings_map, movies_map, "260")
//   => List(Star Wars: Episode V - The Empire Strikes Back (1980), 
//           Star Wars: Episode VI - Return of the Jedi (1983))

// recommendations(ratings_map, movies_map, "2")
//   => List(Lion King, Jurassic Park (1993))

// recommendations(ratings_map, movies_map, "0")
//   => Nil

// recommendations(ratings_map, movies_map, "1")
//   => List(Shawshank Redemption, Forrest Gump (1994))

// recommendations(ratings_map, movies_map, "4")
//   => Nil  (there are three ratings for this movie in ratings.csv but they are not positive)     



}

#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Randomly select a door
#' @description
#' `select_door()` randomly selects one of the three doors.
#'
#' @details
#' Simulates the contestant's initial door choice
#'
#' @param 
#' No arguments are used by this function.
#'
#' @return 
#' An integer between 1 and 3 indicating the door selected.
#'
#' @examples
#' select_door ()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Hosts opens a goat door
#'
#' @description
#' `open_goat_door()` selects one of the doors with a goat that was not picked by the contestant.
#'
#' @details
#' If the contestant initially picks the car, the host randomly chooses one of the two goat doors.
#' If the contestant initially picks a goat, the host must open the other goat door.
#'
#' @param 
#' game A character vector (length 3) with "goat"/"car" values, from `create_game ()`
#' a.pick  An integer (1-3) indicating the contestant's first choice.
#'
#' @return
#' An integer (1-3) indicating the door that was opened by the host.
#' 
#' @examples
#' g <- create_game()
#' p <- select_door()
#' open_goat_door (g,p)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Switch or stay with the chosen door
#'
#' @description
#' `change_door() determines the contestant's final choice based on their strategy
#'
#' @details
#' If the player stays, they keep their first pick.
#' If the player switches, they pick the unopened door that is not their first pick and not the opened goat door.
#
#' @param 
#' stay Logical, whether the player stays with the original pick(`True`)
#' @param 
#' opened.door An integer (1-3), the door opened by the host.
#' @param
#' a.pick An integer (1-3), the contestant's initial pick.
#'
#' @return
#' An integer (1-3) indicating the contestant's final door choice.
#' 
#' @examples
#' g <- create_game()
#' p <- select_door()
#' o <- open_goat_door(g, p)
#' change_door(stay =FALSE, opened.door =o, a,pick = p)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine the game outcome.
#'
#' @description
#' `determine_winner()` determines the contestant wins a car or loses.
#'
#' @details
#' Compares the contestant's final pick with the game setup.
#' @param 
#' final.pick An integer (1-3), the contestant's final door choice.
#' @param
#' game A character vector( length 3) with "goat"/"car" values.
#'
#' @return
#' A string: "WIN" if the final pick is the car, "LOSE" otherwise.
#'
#' @examples
#' g <- create_game()
#' p <- select_door()
#' o <-open_goat_door(g, p)
#' f <- change_door(FALSE, o, p)
#' determine_winner(f, g)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play one round of the Monty Hall game
#'
#' @description
#' `play_games()` determines the contestant wins a car or loses.
#'
#' @details
#' This function wraps all the steps: create a game, select a door, open a goat door 
#' and decide whether to stay or witch. It returns the outcomes for both strategies.
#'
#' @param 
#' No arguments are used by this function.
#'
#' @return 
#' A data frame with two rows:
#' `strategy` : "stay" or "switch"
#' `outcome` : "WIN" or "LOSE"
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play multiple Monty Hall games.
#'
#' @description 
#' `play_n_games()` simulates a specified number of Monty Hall games
#'  by repeatedly calling the `play_game()` function and storing the results.
#'
#' @details
#' This function runs `n independent simulations of the Monty Hall Problem.
#' For each game, both strategies( staying with the initial.pick and switching
#' to the other door) are evaluated. The results are combined into a single data
#' frame, and a summary table showing the proportion of wins and losses for each
#' strategy is printed.
#'
#' @param
#' An integer specifying the number of games to simulate.
#' Defaults to 100.
#' 
#' @return 
#' A data frame containing the strategy used ("stay" or "switch") and the
#' corresponding outcome ("WIN" or "LOSE") for each game played.
#'
#' @examples
#' # Simulates 50 Monty Hall games 
#' results <- play_n_games(50)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

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
#'   Select a door for the Monty Hall game.
#'
#' @description
#'   `select_door()` randomly selects one of the three doors in the
#'   Monty Hall game.
#'
#' @details
#'   The function randomly selects one door from the three available
#'   doors. The selected door represents the contestant's initial choice.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A numeric value representing the door selected by the
#'   contestant. The value will be 1, 2, or 3.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door in the Monty Hall game.
#'
#' @description
#'   `open_goat_door()` determines which door the host should open
#'   after the contestant makes an initial selection.
#'
#' @details
#'   If the contestant selects the car, the function randomly selects
#'   one of the two goat doors. If the contestant selects a goat, the
#'   function opens the remaining door that contains a goat.
#'
#' @param game A character vector representing the game, with two
#'   goats and one car.
#' @param a.pick A numeric value representing the contestant's
#'   initial door selection.
#'
#' @return A numeric value representing the door opened by the host.
#'   The value will be 1, 2, or 3.
#'
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   open_goat_door(game, a.pick)
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
#'   Determine the contestant's final door selection.
#'
#' @description
#'   `change_door()` determines the contestant's final door selection
#'   based on whether the contestant stays with the original choice
#'   or switches to the other unopened door.
#'
#' @details
#'   If `stay` is TRUE, the function returns the contestant's original
#'   door selection. If `stay` is FALSE, the function returns the
#'   remaining unopened door.
#'
#' @param stay A logical value indicating whether the contestant stays
#'   with the original door selection. TRUE means stay and FALSE means
#'   switch.
#' @param opened.door A numeric value representing the door opened by
#'   the host.
#' @param a.pick A numeric value representing the contestant's original
#'   door selection.
#'
#' @return A numeric value representing the contestant's final door
#'   selection. The value will be 1, 2, or 3.
#'
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   opened.door <- open_goat_door(game, a.pick)
#'   change_door(TRUE, opened.door, a.pick)
#'   change_door(FALSE, opened.door, a.pick)
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
#'   Determine the winner of the Monty Hall game.
#'
#' @description
#'   `determine_winner()` determines whether the contestant wins or
#'   loses based on the final door selection and the game setup.
#'
#' @details
#'   The function compares the contestant's final door selection with
#'   the game. If the selected door contains the car, the function
#'   returns "WIN". If the selected door contains a goat, it returns
#'   "LOSE".
#'
#' @param final.pick A numeric value representing the contestant's
#'   final door selection.
#' @param game A character vector representing the game, with two
#'   goats and one car.
#'
#' @return A character value of either "WIN" or "LOSE".
#'
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   opened.door <- open_goat_door(game, a.pick)
#'   final.pick <- change_door(TRUE, opened.door, a.pick)
#'   determine_winner(final.pick, game)
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
#'   Play a complete Monty Hall game.
#'
#' @description
#'   `play_game()` simulates a complete Monty Hall game and compares
#'   the outcome of staying with the original door selection to the
#'   outcome of switching doors.
#'
#' @details
#'   The function creates a new game, randomly selects an initial
#'   door, opens a goat door, determines the final selection for both
#'   the stay and switch strategies, and determines the outcome of
#'   each strategy.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A data frame containing two rows showing the strategy
#'   ("stay" or "switch") and the resulting outcome ("WIN" or "LOSE").
#'
#' @examples
#'   play_game()
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
#'   Play multiple Monty Hall games.
#'
#' @description
#'   `play_n_games()` simulates a specified number of Monty Hall games
#'   and returns the results for the stay and switch strategies.
#'
#' @details
#'   The function runs `play_game()` repeatedly for the specified
#'   number of games. It combines the results into a data frame and
#'   prints the proportion of wins and losses for each strategy.
#'
#' @param n A numeric value indicating the number of games to simulate.
#'   The default is 100 games.
#'
#' @return A data frame containing the strategy and outcome for each
#'   simulated game.
#'
#' @examples
#'   results <- play_n_games(10)
#'   head(results)
#'
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

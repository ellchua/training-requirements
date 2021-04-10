########################
## Defining Functions ##
########################

# board() displays the tic-tac-toe board
board <- function(mat) {
  cat("|",mat[1],"|", mat[2], "|", mat[3],"|", "\n")
  cat("|---+---+---|", "\n")
  cat("|",mat[4],"|", mat[5], "|", mat[6],"|", "\n")
  cat("|---+---+---|", "\n")
  cat("|",mat[7],"|", mat[8], "|", mat[9],"|", "\n")
}

# check_valid() sanitises the user input for placement of character
check_valid <- function(mat, symbol){
  while(TRUE){
    cat(paste0("Where would you like to place ",symbol,"? (1-9) "))
    num = readLines(con = con, n = 1)
    if (!num %in% seq(1:9)) {
      cat("Invalid selection. Please pick a number between 1 and 9.", "\n")
      next()
    } else if(mat[as.numeric(num)] == "X" | mat[as.numeric(num)] == "O"){
      cat("Invalid selection. There's already something there.", "\n")
      next()
    } else {
      break()
    }
  }
  return(as.numeric(num))
}


# update_board() updates the board with user input
update_board <- function(num, mat, symbol){
  mat[num] = symbol
  return(mat)
}

# human_turn() simulates the player's turn (only called after user input is sanitised)
human_turn <- function(num, mat, symbol){
  mat = update_board(num, mat, symbol)
  cat("Move placed!","\n")
  cat("Current board: ", "\n")
  return(mat)
}

# computer_turn() simulates the computer's turn (only called after user input is sanitised)
computer_turn <- function(mat, symbol){
  cat("Computer's turn...", "\n")
  if(is.numeric(mat)){   # if the matrix is numeric, then no X and/or O exists yet.
    num = sample(mat, 1) # pick any of the elements of mat
    mat = update_board(num, mat, symbol) 
    return(mat)
  } else {
    if(length(which(mat == "X" | mat == "O")) == 8){ # if there is only 1 free "space" left, use that "space"
      num = as.numeric(mat[-which(mat == "X" | mat == "O")])
      mat = update_board(num, mat, symbol)
      return(mat)
    } else {
      num = sample(as.numeric(mat[-which(mat == "X" | mat == "O")]), 1) # otherwise, sample 1 "space" from the remaining free "spaces"
      mat = update_board(num, mat, symbol)
      return(mat)
    }
  }
  Sys.sleep(1)
  cat("Current board:", "\n")
}

# did_someone_win() checks if there are any combinations of 3 Xs or Os in a row/column/diagonal or if the board is full and there are no winners (a tie).
did_someone_win <- function(mat){ 
  winner = FALSE
  for(i in 1:length(winning_combinations)){
    if(sum(which(mat == "X") %in% winning_combinations[[i]]) >= 3){
      cat("X wins! Good game.", "\n")
      winner = TRUE
      next()
    } else if (sum(which(mat == "O") %in% winning_combinations[[i]]) >= 3){
      cat("O wins! Good game.", "\n")
      winner = TRUE
    } 
  }
  if (sum(mat == as.character(1:9)) == 0 && winner == FALSE){
    cat("It's a tie! Good game.", "\n")
    winner = TRUE
  }
  return(winner)
}

# play() is the main "game engine"
play <- function(mat, symbol){
  if (symbol == "X"){ # if player chooses X => player first, then computer
    cat("Current Board: ", "\n")
    board(mat)
    while(end == FALSE){
      num = check_valid(mat, symbol)
      mat = human_turn(num, mat, symbol)
      board(mat)
      Sys.sleep(1)
      end = did_someone_win(mat)
      if(end){
        break
      }
      mat = computer_turn(mat, "O")
      board(mat)
      end = did_someone_win(mat)
      if(end){
        break
      }
    }
  } else { # if player chooses O => computer first, then player
    while(end == FALSE){
      mat = computer_turn(mat, "X")
      board(mat)
      end = did_someone_win(mat)
      if(end){
        break
      }
      num = check_valid(mat, symbol)
      mat = human_turn(num, mat, symbol)
      board(mat)
      Sys.sleep(1)
      end = did_someone_win(mat)
      if(end){
        break
      }
    }
  }
}

###############
## Variables ##
###############
mat = matrix(1:9, nrow = 3, ncol = 3)
interactive = 1
winning_combinations = list(c(1,2,3), c(4,5,6), c(7,8,9), c(1,4,7), c(2,5,8), c(3,6,9), c(1,5,9), c(3,5,7))
end = FALSE

###############
## Main Game ##
###############

if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

cat("Hi, what's your name? ")
name <- readLines(con = con, n = 1)

cat(paste0("Hi ", name,", "))

# sanitises user input for tic-tac-toe symbols
while(TRUE){
  cat("X or O? ")
  symbol <- readLines(con = con, n = 1)
  upper_symbol <- toupper(symbol)
  if (upper_symbol != "X" & upper_symbol != "O"){
    cat("Invalid character. Please pick from X or O.", "\n")
    next()
  } else{
    symbol <- upper_symbol
    break
  }
}

play(mat,symbol)


library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day04.txt")

calls <- my_dat[1] %>% str_split(",") %>% unlist() %>% as.numeric()

boards <- my_dat[3:length(my_dat)]


cards <- list()
cardfill <- list()
dummycard <- matrix(0, nrow = 5, ncol = 5)
k <- 0

for(i in seq(from = 1, to = 595, by = 6)){
  k <- k+1
  my_board <- boards[i:(i+4)] %>% str_c(sep = " ") %>% str_extract_all("\\d+") %>%
    unlist() %>% as.numeric() %>% matrix(nrow = 5, byrow = T)
  
  cards[[k]] <- my_board
  cardfill[[k]] <- dummycard
}

cardfill2 <- cardfill

get_spot <- function(my_mat, k){
  return(which(my_mat == k))
}

for(i in 1:length(calls)){
  my_call <- calls[i]
  matches <- lapply(X = cards, FUN=get_spot, my_call) #find all the boards it hits
  for(j in 1:100){ #fill in the matches
    if(length(matches[[j]]) == 1)
      cardfill[[j]][matches[[j]]] <- 1
  }
  #check for a winner
  rowWins <- lapply(lapply(cardfill, rowSums), max) %>% unlist()
  colWins <- lapply(lapply(cardfill, colSums), max) %>% unlist()
  
  if(any(rowWins == 5)){
    wc <- which(rowWins == 5)
    break
  }
  if(any(colWins == 5)){
    wc <- which(colWins == 5)
    break
  }
}

#last call times missing values from winning card
my_call * sum(cards[[wc]][cardfill[[wc]] == 0])


##part 2, last to win

for(i in 1:length(calls)){
  my_call <- calls[i]
  matches <- lapply(X = cards, FUN=get_spot, my_call) #find all the boards it hits
  for(j in 1:length(cards)){ #fill in the matches
    if(length(matches[[j]]) == 1)
      cardfill2[[j]][matches[[j]]] <- 1
  }
  #check for a winner
  rowWins <- lapply(lapply(cardfill2, rowSums), max) %>% unlist()
  
  if(any(rowWins == 5)){
    wc <- which(rowWins == 5)
    cards <- cards[-wc]
    cardfill2 <- cardfill2[-wc]
  }
  if(length(cards) == 1) break
 
  colWins <- lapply(lapply(cardfill2, colSums), max) %>% unlist()
  if(any(colWins == 5)){
    wc <- which(colWins == 5)
    cards <- cards[-wc]
    cardfill2 <- cardfill2[-wc]
  }
  if(length(cards) == 1) break
}

lc <- cards[[1]]
lcf <- cardfill2[[1]]

for(k in (i+1):length(calls)){
  if(any(lc == calls[k])){
    lcf[which(lc == calls[k])] <- 1
    if(any(colSums(lcf) == 5)) break
    if(any(rowSums(lcf) == 5)) break
  }
}

calls[k] * sum(lc[lcf == 0])

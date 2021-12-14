library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day11.txt") %>%
  str_extract_all(".{1}") %>% unlist() %>% as.numeric() %>%
  matrix(nrow = 10, byrow = T)

#part 1

counter <- 0

for(i in 1:100){
  expl_mat <- matrix(0, nrow = 10, ncol = 10)
  
  #first increment neergy
  my_dat <- my_dat + 1
  
  #next
  while(TRUE){
    #find explosions
    explosions <- which(my_dat > 9)
    unexploded <- which(expl_mat == 0)
    to_explode <- explosions[explosions %in% unexploded]
    if(length(to_explode) == 0) break
    
    expl_mat[to_explode] <- 1
    
    for(j in 1:length(to_explode)){
      my_cell <- to_explode[j]
      neighbors <- my_cell + c(-11, -10, -9, -1, 1, 9, 10, 11)
      if(my_cell %% 10 == 1) neighbors <- my_cell + c(-10, -9, 1, 10, 11)
      if(my_cell %% 10 == 0) neighbors <- my_cell + c(-11, -10, -1, 9, 10)
      neighbors <- neighbors[which(neighbors > 0)]
      neighbors <- neighbors[which(neighbors <= 100)]
      
      my_dat[neighbors] <- my_dat[neighbors] + 1
    }
  }
  counter <- counter + sum(my_dat > 9)
  my_dat[which(my_dat > 9)] <- 0
}
print(counter)


###part 2

my_dat <- read_lines("2021/dat/day11.txt") %>%
  str_extract_all(".{1}") %>% unlist() %>% as.numeric() %>%
  matrix(nrow = 10, byrow = T)


counter <- 0

while(TRUE){
  counter <- counter + 1
  expl_mat <- matrix(0, nrow = 10, ncol = 10)
  
  #first increment neergy
  my_dat <- my_dat + 1
  
  #next
  while(TRUE){
    #find explosions
    explosions <- which(my_dat > 9)
    unexploded <- which(expl_mat == 0)
    to_explode <- explosions[explosions %in% unexploded]
    if(length(to_explode) == 0) break
    
    expl_mat[to_explode] <- 1
    
    for(j in 1:length(to_explode)){
      my_cell <- to_explode[j]
      neighbors <- my_cell + c(-11, -10, -9, -1, 1, 9, 10, 11)
      if(my_cell %% 10 == 1) neighbors <- my_cell + c(-10, -9, 1, 10, 11)
      if(my_cell %% 10 == 0) neighbors <- my_cell + c(-11, -10, -1, 9, 10)
      neighbors <- neighbors[which(neighbors > 0)]
      neighbors <- neighbors[which(neighbors <= 100)]
      
      my_dat[neighbors] <- my_dat[neighbors] + 1
    }
  }
  if(sum(my_dat > 9) == 100) break
  my_dat[which(my_dat > 9)] <- 0
}
print(counter)

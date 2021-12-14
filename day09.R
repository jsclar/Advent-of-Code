library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day09.txt") %>% str_extract_all(".{1}") %>%
  lapply(as.numeric) 

dm <- t(matrix(unlist(my_dat), nrow = 100, ncol = 100))
mm <- matrix(0, nrow = 100, ncol = 100)

#corners
if(dm[1,1] < dm[1,2] & dm[1,1] < dm[2,1]) mm[1,1] <- 1
if(dm[1,100] < dm[1,99] & dm[1,100] < dm[2,100]) mm[1,100] <- 1
if(dm[100,1] < dm[100,2] & dm[100,1] < dm[99,1]) mm[100,1] <- 1
if(dm[100,100] < dm[100,99] & dm[100,100] < dm[99,100]) mm[100,100] <- 1

#edges
for(i in 2:99){
  #top
  if(dm[1, i] < dm[1, i-1] & dm[1,i] < dm[1,i+1] & dm[1,i] < dm[2,i]) mm[1,i] <- 1
  
  #bottom
  if(dm[100, i] < dm[100, i-1] & dm[100,i] < dm[100,i+1] & dm[100,i] < dm[99,i]) mm[100,i] <- 1
  
  #left
  if(dm[i,1] < dm[i-1, 1] & dm[i, 1] < dm[i+1, 1] & dm[i, 1] < dm[i, 2]) mm[i, 1] <- 1
  
  #right
  if(dm[i,100] < dm[i-1, 100] & dm[i, 100] < dm[i+1, 100] & dm[i, 100] < dm[i, 99]) mm[i, 100] <- 1
  
}

#center
for(i in 2:99){
  for(j in 2:99){
    if(dm[i, j] >= dm[i-1, j]) next
    if(dm[i, j] >= dm[i+1, j]) next
    if(dm[i, j] >= dm[i, j-1]) next
    if(dm[i, j] >= dm[i, j+1]) next
    mm[i,j] <- 1
  }
}

sum(mm) + sum(dm[which(mm == 1)])

#### part 2

find_basin <- function(my_map, my_col, my_row, my_visited = tibble()){
  my_visited %<>% bind_rows(tibble(x = my_col, y = my_row))
  
  ###hunt up
  if(my_map[my_row - 1, my_col] == 0 & nrow(my_visited %>% filter(y == my_row - 1 & x == my_col)) == 0)
    my_visited <- find_basin(my_map, my_col, my_row - 1, my_visited)
  
  ###hunt down
  if(my_map[my_row + 1, my_col] == 0 & nrow(my_visited %>% filter(y == my_row + 1 & x == my_col)) == 0)
    my_visited <- find_basin(my_map, my_col, my_row + 1, my_visited)
  
  ###hunt left
  if(my_map[my_row, my_col - 1] == 0 & nrow(my_visited %>% filter(y == my_row & x == my_col - 1)) == 0)
    my_visited <- find_basin(my_map, my_col - 1, my_row, my_visited)
  
  ###hunt up
  if(my_map[my_row, my_col + 1] == 0 & nrow(my_visited %>% filter(y == my_row & x == my_col + 1)) == 0)
    my_visited <- find_basin(my_map, my_col + 1, my_row, my_visited)
  
  return(my_visited)
}

bm <- matrix(0, nrow = 100, ncol = 100)
bm[which(dm == 9)] <- -99


wm <- matrix(-99, nrow = 102, ncol = 102)

wm[2:101, 2:101] <- bm #pad out basin edges so there are no corner cases and everything is interior

basin_counter <- 0

while(any(wm == 0)){
  basin_counter <- 1 + basin_counter #update counter
  unmapped <- which(wm == 0)
  
  seed <- unmapped[1] #start in the earliest thing not in a basin

  seed_col <- 1 + floor(seed/102) #turn into r indices
  seed_row <- seed %% 102
  if(seed_row == 0) seed_row <- 102
  
  #crawl out from seed to fill in the basin
  my_basin <- find_basin(wm, seed_col, seed_row)
  for(i in 1:nrow(my_basin)){
    wm[my_basin$y[i], my_basin$x[i]] <- basin_counter
  }
}

my_sizes <- sapply(1:basin_counter, function(x)sum(wm == x))

sort(my_sizes, decreasing = T)[1:3] %>% prod()
library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day07.txt") %>% str_extract_all("\\d+") %>% 
  unlist() %>% as.numeric()


#part 1
sapply(min(my_dat):max(my_dat), function(x)sum(abs(my_dat - x))) -> fuel

min(fuel)

##part 2
fuel2 <- c()
for(i in min(my_dat):max(my_dat)){
  my_dist <- abs(my_dat-i)
  fu <- sapply(my_dist, function(x)sum(0:x))
  fuel2 <- c(fuel2, sum(fu))
}

min(fuel2)
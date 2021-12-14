library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day06.txt") %>% str_extract_all("\\d+") %>% 
  unlist() %>% as.numeric()

count_spawn <- function(mt, days_left){
  if(days_left == 0) return(1)
  
  tn <- mt - 1
  if(tn >= 0) return(count_spawn(tn, days_left-1))
  
  return(count_spawn(6, days_left-1) + count_spawn(8, days_left-1))
}

sbt <- c()
#part 1
for(i in 1:5){
  sbt <- c(sbt, count_spawn(i, 80))
}

sum(table(my_dat) * sbt)

#part 2
fish <- sapply(0:8, function(x)sum(my_dat == x))

advance_fish <- function(my_fish){
  nf <- my_fish[2:9]
  nf[7] <- nf[7] + my_fish[1]
  nf[9] <- my_fish[1]
  return(nf)
}

##part 1 redux
mf1 <- fish
for(i in 1:80)
  mf1 <- advance_fish(mf1)

sum(mf1)

##part 2 
options(scipen = 999)
mf2 <- fish %>% as.numeric()
for(i in 1:256)
  mf2 <- advance_fish(mf2)

sum(mf2)
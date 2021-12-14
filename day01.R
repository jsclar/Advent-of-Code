library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day01.txt")

my_dat <- as.numeric(my_dat)

#part 1: change in depth
d1 <- my_dat[-1]
d2 <- my_dat[-length(my_dat)]

sum((d1-d2) > 0) #solution 1

#part 2: windows
dep1 <- my_dat[-c(length(my_dat)-1, length(my_dat))]
dep2 <- my_dat[-c(1, length(my_dat))]
dep3 <- my_dat[-c(1, 2)]

windows <- dep1 + dep2 + dep3

w1 <- windows[-1]
w2 <- windows[-length(windows)]

sum((w1-w2) > 0) #solution 2
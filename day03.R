library(tidyverse)
library(magrittr)

my_dat <- tibble(raw = read_lines("2021/dat/day03.txt"))



bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

### part 1
epsilon <- gamma <- c()
for(i in 1:nchar(my_dat$raw[1])){
 my_char <- str_sub(my_dat$raw, i, i) %>% as.numeric() %>% table()
 
 gb <- names(my_char)[which.max(my_char)] %>% as.numeric()
 gamma <- c(gamma, gb)
 epsilon <- c(epsilon, 1-gb)
}

bitsToInt(gamma) * bitsToInt(epsilon)

### part 2
oxygen <- carbon <- my_dat$raw 
i <- 0
while(TRUE){
  i <- i + 1
  if(i > nchar(oxygen[1])) i <- 1
  #oxygen is most common
  if(length(oxygen) > 1){
    rc <- str_sub(oxygen, i, i)
    counter <- table(rc)
    if(length(counter) == 1)
      my_filter <- rc == names(counter)
    if(length(counter) == 2 & counter[2] >= counter[1])
      my_filter <- rc == "1"
    if(length(counter) == 2 & counter[1] > counter[2])
      my_filter <- rc == "0"
    oxygen <- oxygen[my_filter]
  }
  #carbon is least common
  if(length(carbon) > 1){
    rc <- str_sub(carbon, i, i)
    counter <- table(rc)
    if(counter[2] >= counter[1])
      my_filter <- rc == "0"
    if(counter[1] > counter[2])
      my_filter <- rc == "1"
    carbon <- carbon[my_filter]
  } 
  
  if(length(carbon) == 1 & length(oxygen) == 1) break

}


o10 <- unlist(strsplit(oxygen, split = "")) %>% as.numeric()
c10 <- unlist(strsplit(carbon, split = "")) %>% as.numeric()

bitsToInt(o10) * bitsToInt(c10)

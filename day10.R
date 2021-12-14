library(tidyverse)
library(magrittr)

my_dat <- read_lines("2021/dat/day10.txt") 

invalid <- bad <- c()

incomplete <- list()

for(i in 1:length(my_dat)){
  invalid_flag <- F
  my_str <- my_dat[i]
  my_open <- c()
  
  for(j in 1:nchar(my_str)){
    nl <- str_sub(my_str, j, j)
    
    #if its a new start, add it to the stack of starts
    if(nl %in% c("(", "[", "<", "{")){
      my_open <- c(my_open, nl)
      next
    }
    
    #if its a close
    if(nl %in% c(")", "]", ">", "}")){
      #check if it matches the next thing to close, if it does, pop it and move on
      if(str_c(my_open[length(my_open)], nl) %in% c("()", "[]", "<>", "{}")){
        my_open <- my_open[-length(my_open)]
        next
      }
      #otherwise, store the bad character, note that the line is invalid, break
      bad <- c(bad, nl)
      invalid <- c(invalid, i)
      invalid_flag <- T
      break
    }
  }
  if(invalid_flag == F) incomplete[[length(incomplete) + 1]] <- rev(my_open)
}

3 * sum(bad == ")") + 57 * sum(bad == "]") + 1197 * sum(bad == "}") + 25137 * sum(bad == ">")

### part 2
my_scores <- c()

for(i in 1:length(incomplete)){
  ls <- 0
  my_seq <- incomplete[[i]]
  seq_score <- as.numeric(my_seq == "(") + 2* as.numeric(my_seq == "[") + 3*as.numeric(my_seq == "{") + 4*as.numeric(my_seq == "<")
  for(j in 1:length(seq_score)){
    ls <- ls*5
    ls <- ls + seq_score[j]
  }
  my_scores <- c(my_scores, ls)
}

median(my_scores)
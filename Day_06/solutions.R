# AoC 2021: Day 6
library(tidyverse)
library(magrittr)
options(scipen=999)

# a. read input
input <- readLines('Day_06/input.txt') %>% str_split(',', simplify=T) %>% as.numeric()
test <- c(3,4,3,1,2)

# b. create population tracker
pop <- rep(0, 9) %>%
       as.list() %>%
       set_names(paste0('D', 8:0))

# c. summarize initial population
init <- input
init %<>% table() %>% as.list() %>% set_names(paste0('D', names(.)))

# d. populate pop tracker
common <- intersect(names(init), names(pop))
lapply(common, \(x) pop[x] <<- init[x]) %>% invisible()

# e. make it happen -- not very proud of this, but it does work...
sim <- function(x) {
                    newborns <- x$D0
                    x$D0 <- x$D1
                    x$D1 <- x$D2
                    x$D2 <- x$D3
                    x$D3 <- x$D4
                    x$D4 <- x$D5
                    x$D5 <- x$D6
                    x$D6 <- x$D7 + newborns
                    x$D7 <- x$D8
                    x$D8 <- newborns
                    x
                }

replicate(n=256, expr={ pop <<- sim(pop) }, simplify=F)[[256]] %>% unlist() %>% sum()



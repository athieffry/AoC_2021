# Day 07
library(tidyverse)
library(magrittr)

# q1
horizon <- readLines('input.txt') %>%
           str_split(',', simplify=T) %>%
           as.numeric()

alns <- 0:max(horizon)
delta <- sapply(horizon, \(x) abs(alns-x))

delta %>% rowSums() %>% min()

# q2
delta %>%
    apply(1, \(x) sapply(x, \(y) sum(1:y))) %>%
    colSums() %>%
    min()





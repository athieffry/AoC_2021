# Day 03
library(tidyverse)
library(magrittr)
library(reshape2)

# q1
input <- readLines('input.txt')
n_bits <- nchar(input[1])

mat <- input %>%
       str_split(pattern='', simplify=T) %>%
       as.numeric() %>%
       matrix(ncol=n_bits)

get_major_bits <- function(m) { ifelse(colSums(m) >= nrow(m)/2, 1, 0) }
get_minor_bits <- function(m) { ifelse(colSums(m) < nrow(m)/2, 1, 0) }
bit_to_decimal <- function(b) { paste0(b, collapse='') %>% strtoi(base=2) }

get_major_bits(m=mat) %>% bit_to_decimal() * get_minor_bits(m=mat) %>% bit_to_decimal()

# q2
get_q2 <- function(m, type='major') { for (i in 1:ncol(m)) {
                                                            tokeep <- ifelse(type=='major', get_major_bits(m)[i], get_minor_bits(m)[i])
                                                            m <- m[m[, i]==tokeep, ]
                                                            if(is.null(nrow(m))) { return(bit_to_decimal(m))
                                                                                   break()}
                                    }}

get_q2(mat, type='major') * get_q2(mat, type='minor')


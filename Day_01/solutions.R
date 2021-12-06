# Advent of Code 2021 - DAY 01
# ----------------------------
library(tidyverse)
# read input
input <- read.table('Day_01/input.txt')$V1
# q1
sum(input - lag(input) > 0, na.rm=T) # 1288
# q2
wsum <- zoo::rollapply(input, width=3, by=1, FUN=sum)
sum(wsum - lag(wsum) > 0, na.rm=T) # 1311

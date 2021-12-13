# Day 13
library(tidyverse)
library(magrittr)
library(stringr)

# a. read and parse input
input <- readLines('Day_13/input.txt')

foldings <- input %>%
            str_subset('fold') %>%
            str_remove('fold along ') %>%
            str_split('=', simplify=T) %>%
            as_tibble() %>%
            set_colnames(c('dir', 'loc')) %>%
            mutate('loc'=as.numeric(loc)+1)

df <- input %>%
      str_subset(',') %>%
      str_split(',', simplify=T) %>%
      as_tibble() %>%
      mutate_all(as.numeric) %>%
      mutate_all(~add(., 1)) %>%
      set_colnames(c('x', 'y'))

# b. create initial empty matrix with max dimensions encountered
max_x <- max(df$x)
max_y <- max(df$y)
mat <- matrix(rep(0, max_x * max_y), ncol=max_x)

# c. populate initial matrix
apply(df, 1, \(x) mat[x[2], x[1]] <<- 1 )

# d. folding functions
fold_y <- function(mat, yline) {
    # get upper matrix
    up <- mat[1:(yline-1), ]
    dim(up)
    # get lower matrix
    down <- mat[(yline+1):nrow(mat), ] %>% apply(2, rev)
    dim(down)
    # get row number delta
    delta <- nrow(up) - nrow(down)
        # CASE 1: both matrix of same height
        if(delta==0) { return(up + down) }
        # CASE 2: upper matrix is bigger
        if(delta > 0) {
                       buffer <- matrix(rep(0, ncol(mat) * abs(delta)), ncol=ncol(mat))
                       down <- rbind(buffer, down)
                       return(up + down)
                      }
        # CASE 3: upper matrix is smaller
        if(delta < 0) {
                       buffer <- matrix(rep(0, ncol(mat) * abs(delta)), ncol=ncol(mat))
                       up <- rbind(up, buffer)
                       return(up + down)
                      }
    }


fold_x <- function(mat, xline) {
    # get left matrix
    left <- mat[ ,1:(xline-1)]
    # get right matrix
    right <- mat[ ,(xline+1):ncol(mat)] %>% apply(1, rev) %>% t()
    # get column number delta
    delta <- ncol(left) - ncol(right)
        # CASE 1: both matrix of same width
        if(delta==0) { return(left + right) }
        # CASE 2: left matrix bigger width
        if(delta > 0) {
                       buffer <- matrix(rep(0, nrow(mat) * abs(delta)), ncol=abs(delta))
                       right <- cbind(buffer, right)
                       return(left + right)
                      }
        # CASE 3: left matrix smaller width
        if(delta < 0) {
                       buffer <- matrix(rep(0, nrow(mat) * abs(delta)), ncol=abs(delta))
                       left <- cbind(left, buffer)
                       return(left + right)
        }
}

# e. Question 1:
mat_q1 <- mat
apply(foldings[1,], 1, \(x) ifelse(x[1]=='x',
                                   mat_q1 <<- fold_x(mat_q1, xline=as.numeric(x[2])),
                                   mat_q1 <<- fold_y(mat_q1, yline=as.numeric(x[2]))) )
sum(mat!=0)

# f. Question 2:
apply(foldings, 1, \(x) {
                        ifelse(x[1]=='x',
                               mat <<- fold_x(mat, xline=as.numeric(x[2])),
                               mat <<- fold_y(mat, yline=as.numeric(x[2])))
                        })

# g. plot password
mat[mat > 1] <- 1
pheatmap::pheatmap(mat2, cluster_rows=F, cluster_cols=F, cellwidth=5, cellheight=5, color=c('white', 'black'), legend=F, border_color=NA)

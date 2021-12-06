library(tidyverse)
library(magrittr)


# q2.1
input <- read.table('input.txt') %>%
         set_colnames(c('dir', 'val')) %>%
         as_tibble()

q1 <- input %>%
      group_by(dir) %>%
      summarize('sum'=sum(val)) %>%
      deframe()

q1['forward'] * ( q1['down'] - q1['up'] ) %>% as.vector()

# q2.2
input <- read.table('input.txt') %>% set_colnames(c('dir', 'val'))

horizon <- aim <- depth <- 0

for (i in 1:nrow(input)) {
  dir <- input$dir[i]
  val <- input$val[i]

  if(dir == 'forward') { horizon <- horizon + val
                         depth <- depth + (aim * val) }
  if(dir == 'down') { aim <- aim + val }
  if(dir == 'up') { aim <- aim - val }

}

horizon * depth

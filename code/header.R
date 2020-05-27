my.library <- '/Users/swnam/Google Drive/code/R/library'
.libPaths(my.library)

set.seed(1234)

library('tidyverse')
c('reshape2', 'stringr', 'magrittr', 'doParallel') %>%
  walk(~library(., character.only=TRUE))

setwd('/Users/swnam/Google Drive/Kellogg/02. Second year/2020-Spring/523-0. Estimation of Dynamic programs/HW06/code')

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

var_save <- './variables/'

registerDoParallel(cores=28)
# --- DATA LOAD ----
setwd("D:/digital_learning/statistic and probability - Duke Coursera/")
#download.file("http://www.openintro.org/stat/data/cc.RData", destfile = "cc.RData")
#load("cc.RData")


load("D:/digital_learning/statistic and probability - Duke Coursera/os3_data/cc.RData")

# --- SAMPLE ----
sample(countyComplete$hispanic, 5)
#campionamento con replacement
sample(countyComplete$hispanic, 5, replace=TRUE)



# --- STRATIFIED SAMPLE WITH DPLYR ----
library(dplyr)
# example from  https://gist.github.com/ramhiser/8b5ffd0ffbfbf1f49e71bbbd330bf72d
# Uses a subset of the Iris data set with different proportions of the Species factor

set.seed(42)
iris_subset <- iris[c(1:50, 51:80, 101:120), ]

stratified_sample <- iris_subset %>%
  group_by(Species) %>%
  mutate(num_rows=n()) %>%
  sample_frac(0.4, weight=num_rows) %>%
  ungroup

# These results should be equal
table(iris_subset$Species) / nrow(iris_subset)
table(stratified_sample$Species) / nrow(stratified_sample)

# Success!
#     setosa versicolor  virginica 
#       0.5        0.3        0.2 


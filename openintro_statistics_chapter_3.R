library(tidyverse)
# install.packages("devtools")
#  library(devtools)
#  devtools::install_github("thomasp85/patchwork")
library(patchwork) # for plotting two charts side by side in ggplot

# creating two distributions for fig 3.2
set.seed(18)
x3_2a<-seq(-3,3, length =100)
x3_2b<-seq(7,31, length= 100)

y3_2a<-dnorm(x3_2a, mean = 0, sd = 1)
y3_2b<-dnorm(x3_2b, mean = 19, sd = 4)

df <- data.frame(x3_2a,y3_2a, x3_2b,y3_2b)

p1 <- ggplot(df) + geom_line(aes(x3_2a, y3_2a))
p2 <- ggplot(df) + geom_line(aes(x3_2b, y3_2b))

p1 + p2
# Give the chart file a name.

png("../images/fig3_2.png")
p1+p2
dev.off()

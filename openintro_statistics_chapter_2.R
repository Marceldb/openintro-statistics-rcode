library(tidyverse)
# (pseudo) random generation of 1.000 rolls
set.seed(13)
tab2_1 <- as.data.frame(sample(x = 1:6, size = 1000, replace = TRUE))

# renaming dataframe col
colnames(tab2_1)[1] <- "outcome"
tab2_1$rolls<-as.numeric(rownames(tab2_1))

# creating success (all outcomes =1) column and probability (p) of success column 
# with n(success)/n(rolls)
tab2_1$success=0
tab2_1$success[which(tab2_1$outcome==1)] = 1
tab2_1$p=cumsum(tab2_1$success)/tab2_1$rolls
summary(tab2_1)

# visualization
fig2_1<-ggplot(tab2_1)+
  geom_line(aes(x=rolls, y=p))+
  geom_hline(yintercept =1/6, linetype="dashed")+
  geom_vline(xintercept=30,color="red" )+
  geom_text( mapping=aes(x=30, y=0),label="30", color="red")+
  theme_classic()
fig2_1



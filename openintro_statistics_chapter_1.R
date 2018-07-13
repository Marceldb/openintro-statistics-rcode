# --- DATA LOAD ----
# to install openintro and datasets
# install.packages("devtools")
library(devtools)
install_github("OpenIntroOrg/openintro-r-package", subdir = "openintro")
library(openintro)
library(tidyverse)
library(magrittr)

# viewing datasets from openintro
data(package="openintro")



#---1.1 Case study: using stents to prevent strokes----
summary(stent30)
summary(stent365)

mytbl<-stent30%>%count(group,outcome)%>%mutate(outcomeTime=30)
mytbl<-rbind(mytbl, stent365%>%count(group,outcome)%>%mutate(outcomeTime=365))

View(mytbl)

pct<-mytbl %>% 
  group_by(group, outcomeTime)%>%
  mutate(total_events=sum(n), pctByTimeGroup=n/total_events*100)

#---1.2 Data Basics----
summary(email50)
View(email50)

tbl1_3<-select(email50, spam,num_char,line_breaks,format, number)
slice(tbl1_3, -4:-49)
str(tbl1_3)
dim(tbl1_3)
colnames(tbl1_3)


# Table 1.5 county dataset
summary(countyComplete)
colnames(countyComplete)
View(countyComplete)

fig_1_8_tbl<-select(countyComplete,name, fed_spending, poverty, pop2010)
fig_1_8_tbl %<>% 
  mutate(fed_spending_percapita=fed_spending/pop2010*100)
fig_1_8_tbl %<>% 
filter(fed_spending_percapita < 3344)



summary(fig_1_8_tbl)


fig_1_8 <-
ggplot(fig_1_8_tbl, aes(poverty, fed_spending_percapita/100))+
  geom_point() +
  labs(x = "Poverty Rate (Percent)")+
  labs(y = "Federal Spending Per Capita") +
  geom_hline(yintercept= 21.5, color="red")+
  geom_vline(xintercept= 41.5, color="red")+
  labs(caption = "Figure 1.8: A scatterplot showing fed spend against poverty. Owsley County of Kentucky, with a poverty rate of 41.5% and federal spending of $21.50 per capita, is highlighted.") +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0.5))
fig_1_8

# Fig. 1.18 Example of nonlinear relation between two variables
# example in the text is shown as a linear one, though
fig_1_18<-ggplot(cars,aes(weight, price))+
  geom_point()+
  geom_smooth(linetype="dashed")+
  labs(x="Weight (Pounds)")+
  labs(y="Price ($1000s)")+
  theme_light()+
  labs(caption = "Figure 1.18: A scatterplot of price versus weight for 54 cars.")+
  theme(plot.caption = element_text(hjust = 0.5))
fig_1_18

# ---1.6.2 Dot plots and the mean----
# Simple Dotplot


fig_1_20<-ggplot(email50, aes(x=num_char)) + 
  geom_dotplot()+
  labs(caption = "Number of Characters (in thousands)")+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0.5))

fig_1_20  


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


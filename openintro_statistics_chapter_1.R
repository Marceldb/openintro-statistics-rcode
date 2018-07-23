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

tbl<-email50 %>% 
    mutate(bin = ntile(num_char, 8)) %>% 
  group_by(bin) %>%
  count() 
?cut_interval
email50 %>% 
  mutate(bin = ntile(num_char, 8)) %>% View()

# --- 1.6.3 HIstograms and Shape ----

# tbl 1.21
# selecting by num_chars and assigning to a segment
# probably can be done easier with binning functions

tbl_1_21<-email50
mybreak<- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65)
# use labels parameter if you want to change the labels such as "5-10" etc
tbl_1_21$segmento<-NA
tbl_1_21$segmento<-cut(email50$num_char, breaks = mybreak)

# or
tbl_1_21$segmento<-NA
tbl_1_21$segmento<-cut(email50$num_char, seq(from =0, to = 65, by = 5))



# table 1.21 tidy version
tbl_1_21 %<>% 
  group_by(segmento) %>% 
  summarise(n = n())


# table 1.21 with a column for 
# any bin, same as the book
tbl_1_21_s<-tbl_1_21 %>% spread(segmento,n)
tbl_1_21_s

# Histogram fig. 1.22
email50 %>% 
  ggplot()+
  geom_histogram(aes(num_char))

bfill <- "skyblue4"
bline <- "grey3"
ggplot(email50, aes(x=num_char)) + 
  geom_histogram( binwidth = 5,colour = bline, fill = bfill)+
  labs(caption = "Figure 1.22: A histogram of num char. This distribution is very strongly skewed to the right.")+
  labs(x = "Number of Characters (in thousands)")+
  labs(y = "Frequency") +
  theme_light()+
  theme(plot.caption = element_text(hjust = 0.5))

#  see also https://rdrr.io/cran/openintro/man/

# Fig 1.29 (a)
MLB %>% 
  ggplot()+
  geom_histogram(aes(salary))

bfill <- "skyblue4"
bline <- "grey3"
ggplot(MLB, aes(x=salary/1000)) + 
  geom_histogram( binwidth = 5,colour = bline, fill = bfill)+
  labs(caption = "Figure 1.29: (a) Histogram of MLB player salaries for 2010, in millions of dollars")+
  labs(x = "Salary (millions of dollars)\n(a)")+
  labs(y = "") +
  theme_light()+
  theme(plot.caption = element_text(hjust = 0.5))


# Fig 1.29 (b)
MLB %>% 
  ggplot()+
  geom_histogram(aes(salary))

bfill <- "skyblue4"
bline <- "grey3"
ggplot(MLB, aes(x=salary/1000)) + 
  geom_histogram( binwidth = 5,colour = bline, fill = bfill)+
  labs(caption = "Figure 1.29: (b) Histogram of the log-transformed MLB player salaries for 2010.")+
  labs(x = "loge(Salary), where Salary is in millions USD\n(b)")+
  labs(y = "") +
  theme_light()+
  theme(plot.caption = element_text(hjust = 0.5))

#----1.6.4 Variance and Standard Deviation----
# Manually calculating deviation as xi-xavg with a for loop
calc.deviation = function(x) {
  avg<-mean(x)
    for (i in 1:50) {
    print(x[i]-avg)
    }
}
calc.deviation(email50$num_char)

# Manually calculating deviation as xi-xavg with r vectorization
calc.deviation = function(x) {
  avg<-mean(x)
  mydeviation<-0
  mydeviation<-(x-avg)
  print(mydeviation)
}

calc.deviation(email50$num_char)

# Sample Variance s2
# manually calculating sample variance s2 with a for loop
calc.smpl.variance = function(x) {
  avg<-mean(x)
  endloop<-length(email50$num_char)
  sv<-0
  for (i in 1:endloop) {
    sv<-sv+(x[i]-avg)^2
    print(sv)
  }
  sv<-sv/(endloop-1)
  mytext<-paste("s2 sample variance is", sv)
  print(mytext)
}
calc.smpl.variance(email50$num_char)
# Manually calculating sample variance s2 with r vectorization
calc.smpl.variance = function(x) {
  avg<-mean(x)
  sv<-0
  mydenominator<-length(x)
  sv<-sum((x-avg)^2)
  sv<-sv/(mydenominator-1)
  mytext<-paste("s2 sample variance is", sv)
  print(mytext)
}
calc.smpl.variance(email50$num_char)

# Check_ calculating sample variance with r function  (n-1 in the denominator)
var(email50$num_char) 
# 172.2725 same as the manual function 
# the book says 172,44 probably an error 

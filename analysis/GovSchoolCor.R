setwd("~/Desktop/UVA PhD/Spring 2018/Quant I")
source("stat-functions (1).R")

gs<-read.csv("GovSchoolStats.csv")

plot(x=gs$d_index, y=gs$GS.Rate)
cor(x=gs$d_index, y=gs$GS.Rate)

# End MRT portion --------------------------

# Begin ACM portion ----------------------------
library(tidyverse)
library(psych)
getwd()
# setwd("C:\\Users\\mccar\\Desktop\\tracking")
gs<-read_csv("GovSchoolStats.csv")


gs <- gs%>% 
  mutate(gsrate = `GS Rate`) 

describe(gs$gsrate)
# How is this calculated? It's not a proportion of gs participation to regular school? 

try(cor(gs$gsrate, gs$d_index, pairwise.complete.obs = TRUE))
# I get the same error even when using the correct argument for dealing with NAs. 

# luckily, there's an easier way to deal with it! 

# Regression to the rescue!

model<-lm(gsrate ~ d_index, data = gs)
summary(model)

# the adjusted R^2 of this model is: 
r2<-0.01114
# so the pearson correlation coefficient is: 
sqrt(r2)  
# r = 0.1055

# let's look at it visually:
gs %>% 
  ggplot(aes(x = d_index, y = gsrate)) +
  geom_point() +
  geom_smooth(method = "lm")


# here are the error messages you get:  
# Warning messages:
#   1: Removed 2 rows containing non-finite values (stat_smooth). 
#   2: Removed 2 rows containing missing values (geom_point). 

# This gives us some insight into your problem: It looks like some of your values
# are missing, which is fine, but some of them are infinity. Just what were you
# doing with your GS Rate?? :) 

# Looks like your data are mostly flat and you have an ENORMOUS outier somewhere. 

# Doesn't seem like a profitable line of inquiry. 

# Can you help me figure out how you calculated your GovSchool Rate? 

# The skew of the y variable suggests a log scaling. Let's take a look:

gs %>% 
  ggplot(aes(x = d_index, y = gsrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

# and see what the log-linear model is: 
nonzeros<-gs %>% 
  filter(gsrate != 0) %>% 
  filter(!(is.na(gsrate)))
model2<-lm(log(gsrate) ~ d_index, data = nonzeros)
summary(model2)
# In this case, you DO have a significant predictor: 
# it looks like ever 100% increase in d_index predicts
# a change in govschool rate of ...  I suck at interpreting
# log linears. 
exp(-1.7516)
# changes your govschool rate by about 0.17? I guess? not a big change. 
# I'm not 100% that this is the correct interpretation.

# .... but your correlation is
r2log<-0.03241
sqrt(r2log)
# pearson r = 0.18

### Albemarle Giftedness 

library(tidyverse)
library(reshape2)
library(readxl)
library(scales)
library(wesanderson)
library(ggthemes)
library(ggmosaic)

setwd("D:\\Data Analysis Software\\Data Sets")
setwd("G:\\Data Analysis Software\\Data Sets")


albemarle<-readxl::read_xlsx("albemarle.xlsx")

# albemarle$unid <- albemarle$division-albemarle$gifted

albemarle<- albemarle %>%
  melt()
#filter(variable != "division")


albemarle
races<-ggplot(albemarle,aes(x = variable, y = value, fill = race)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format())

pal <- wes_palette(5, name = "IsleofDogs1", type = "discrete")
pal

races+scale_fill_manual(values=pal)+
  labs(
    title="Demographic Percentages of Students",
    subtitle = "In Total Divison and Identified as Gifted",
    x = "",
    y = "Percent by Demographic",
    color = "Demographic Groups"
  ) + theme_fivethirtyeight()

albs<-ggplot(data=albemarle) + geom_mosaic(aes(weight=value, x = product(variable), fill = race))
albs+scale_fill_manual(values=pal)+
  labs(
    title="Demographic Percentages of Students",
    subtitle = "In Total Divison and Identified as Gifted",
    x = "",
    y = "Percent by Demographic",
    color = "Demographic Groups"
  )


#####################################
albemarle<- albemarle %>%
  spread(key=variable, value = value)


RCDI<-function(x,y){
  pctgift<-x / sum(x)
  pcttot <-y / sum(y)
  z <- y-x
  pctnon<-z / sum(z)
  rcdi <- (pctgift - pctnon )/ (pctnon) * 100
  return(rcdi)
}
EI<-function(x,y){
  pctgift<-x / sum(x)
  pcttot <-y / sum(y)
  z <- y-x
  pctnon<-(z / sum(z))*100
  part1<- pctnon*0.20
  part2<- pctnon-part1
  return(part2)
}

albemarle
RCDI(albemarle$gifted, albemarle$division)
EI(albemarle$gifted, albemarle$division)
© 2021 GitHub, Inc.

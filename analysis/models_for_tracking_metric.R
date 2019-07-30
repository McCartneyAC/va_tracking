
library(readxl)
library(magrittr)
library(ggthemes)
# devtools::install_github("McCartneyAC/mccrr")
library(mccrr)
library(extrafont)
library(psych)
library(tidyverse)
library(lme4)
library(edlf8360)
library(sjPlot)

setwd("C:\\Users\\mccar\\Desktop\\tracking")
setwd("C:\\Users\\Andrew\\Desktop\\va_tracking-master")
df<-read_csv("analytic_data.csv")

# functions --------------------
# 

theme_textbook <- function () {
  theme_light() + theme(text = element_text(family = "Calibri"))
}

paste_data <- function(header=TRUE,...) {
  require(tibble)
  x<-read.table("clipboard",sep="\t",header=header,stringsAsFactors=TRUE,...)
  as_tibble(x)
}




# re-run models with diversity score --------------------------------

lm(metric ~ urban + mostly + pct_frpl + d_index + log(`Total Enrollment`), data = df) %>% 
  stata_summary()
df %>% 
  lm(pct_adv ~ urban + mostly + pct_frpl + d_index + log(`Total Enrollment`), data = .) %>% 
  stata_summary()
model5<-df %>% 
  lm(pct_adv~ pct_frpl + d_index + log(`Total Enrollment`), data = .)
summary(model5)
model6<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`), data = df)
model9<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + urban + mostly, data = df)
model7<- lm(metric ~ pct_frpl  + log(`Total Enrollment`), data = df)
summary(model6)
sum(is.na(df$`Total Enrollment`))

df %>% 
  filter(!is.na(metric)) %>% 
  mutate(pts = fitted(model7, level = 0, asList = FALSE)) %>% 
  ggplot(aes(y = pts, x = d_index)) +
  geom_point(color = "#E57200", alpha = 0.8, stroke = 0) +
  # geom_text(alpha = 0.8, aes(label = Name.x)) + 
  geom_smooth(color = "#232D4B") + 
  labs(
    title = "Levels of Courses and Diversity", 
    subtitle = "Controlling for FRPL and log of enrollment",
    x = "D - Index (Kelly & Price, 2011)",
    y = "Model Predicted Course Levels"
  ) + 
  theme_textbook()

model8<-lm(metric~d_index, data = df)
mccrr::gg_added_var(partial = model8, extended = model7)
rockchalk::outreg(model6, type = "html")
model10<-df %>% 
  mutate(d_sq = d_index^2) %>%  
  lm(metric ~ d_index + d_sq + pct_frpl + log(`Total Enrollment`) + urban + mostly, data = .)
model11<-df %>% 
  mutate(d_sq = d_index^2) %>%  
  lm(metric ~ d_index + d_sq + pct_frpl + log(`Total Enrollment`), data = .)
model12 <- df %>% 
  lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + urban + mostly + factor(`Division num`), data = .) %>% 
  stata_summary()
model13 <- df %>% 
  lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + factor(`Division num`), data = .) %>% 
  stata_summary()
model14 <- df %>% 
  lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + factor(`Division num`)*urban, data = .) %>% 
  stata_summary()

summary(model10)

# lme models -----------------------------
model0 <- df %>% 
  lmer(metric~ 1 + (1 | `Division num`), data = .)
icc(model0)
model15 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + urban + mostly + (1 | `Division num`), data = .)
model16 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + (1 | `Division num`), data = .)



# Final Prferred models
model6<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`), data = df)
model9<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + urban + mostly, data = df)
model15 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + urban + mostly + (1 | `Division num`), data = .)
model16 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + (1 | `Division num`), data = .)

OLS_restricted<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`), data = df)
OLS_w_Urbanicity<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + urban + mostly, data = df)
LME_restricted<-df %>% 
  rename(total_enrollment = `Total Enrollment`) %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(total_enrollment) + (1 | `Division num`), data = .)
LME_w_Urbanicity<- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + urban + mostly + (1 | `Division num`), data = .)
FE_restricted <-df %>% 
  rename(total_enrollment = `Total Enrollment`) %>% 
  rename(divnum = `Division num`) %>% 
  lm(metric ~ d_index + pct_frpl + log(total_enrollment) + factor(divnum), data = .)
FE_with_Urbanicity<-df %>% 
  rename(total_enrollment = `Total Enrollment`) %>% 
  lm(metric ~ d_index + pct_frpl + log(total_enrollment) + urban + mostly + factor(`Division num`), data = .)

tab_model(OLS_restricted, FE_restricted, LME_restricted)
tab_model(OLS_w_Urbanicity, FE_with_Urbanicity, LME_w_Urbanicity)

library(ggsci)

plot_model(FE_restricted, type = "pred", terms = c("d_index", "divnum"), transform = "total_enrollment" ) + theme_textbook() + geom_point() 






rockchalk::outreg(
  list(
    "OLS " = model6,
    "OLS with Urbanicity"    = model9,
    "MLM"    = model16,
    "MLM with Urbanicity"    = model15
  ), 
  type = "html")


rockchalk::outreg(
  list(
    "Var Comps" = model0,
    "Levels" = model6,
    "Levels" = model9 ,
    "Levels" = model10,
    "Levels" = model11, 
    "Levels" = model12, 
    "Levels" = model13,
    "Levels" = model14
    ), 
  type = "html")



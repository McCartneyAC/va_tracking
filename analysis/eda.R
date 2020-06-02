
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

# setwd("C:\\Users\\mccar\\Desktop\\tracking")
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files"); getwd()

df<-read_csv("analytic_data.csv")

# functions --------------------
# 
uvapal <- c("#E57200","#232D4B", "#007681","#F2CD00","#692A7E", "#84BD00","#A5ACAF", "#5C7F92","#857363","#CAC0B6")
theme_textbook <- function () {
  theme_light() + theme(text = element_text(family = "Times New Roman"))
}

paste_data <- function(header=TRUE,...) {
  require(tibble)
  x<-read.table("clipboard",sep="\t",header=header,stringsAsFactors=TRUE,...)
  as_tibble(x)
}



# else
# 
# clean data ----------------------
df1<-read_xlsx("book2.xlsx")
df2<-read_xlsx("book3.xlsx")
df3<-read_xlsx("book4.xlsx")
spending<-read_xlsx("expenditure.xlsx") 
spending <-spending %>% 
  mutate(id = div_number) %>% 
  select(-`School Division`, -div_number)
# pos<-read_xlsx("pos.xlsx")
# pos<-paste_data()
pos<- read_csv("pos_clean_3.csv")
pos %<>% 
  mutate(id = dist_id) %>% 
  dplyr::select(-dist_id) %>% 
  dplyr::select(id, everything()) %>% 
  dplyr::select(id, SUM, `Division num`) %>% 
  print()

df1 %<>% 
  mutate(id = `dist id`) %>% 
  dplyr::select(-`dist id`)

df2 %<>% 
  mutate(id = `Div. No.`) %>% 
  dplyr::select(-`Div. No.`)

df3 %<>% 
  mutate(id = `division number`) %>% 
  dplyr::select(-`division number`)

df <- df1 %>% 
  left_join(df2, by = "id") %>% 
  left_join(df3, by = "id") %>% 
  select(id, everything()) %>% 
  arrange(id) %>% 
  mutate(census = `Census Total`)

df <- df %>% 
  left_join(pos, by = "id")
pos
df
spending


df <- df %>% 
  left_join(pos, by = "id")


df %>% view()
#gen dummies
df <- df %>% 
  mutate(urban = if_else(urbanicity == "urban", 1, 0)) %>% 
  mutate(mostly = if_else(urbanicity == "mostly_rural", 1, 0)) %>% 
  mutate(rural = if_else(urbanicity == "rural", 1, 0)) %>% 
  mutate(pct_frpl = (`Total FRPL` / `Total Enrollment`)) %>% 
  mutate(pct_apib = (AB_IB_Dual_Enrollment / `Total Enrollment`)) %>% 
  mutate(pct_adv = `n_Advanced`/(`n_Advanced` + `n_ Standard Diploma` + 
                                   `n_Drop out` + GED + `n_ Applied`)) %>% 
  mutate(pct_hisp  = (Hispanic_any / `Total Enrollment`)) %>% 
  mutate(pct_ai_na = (AI_NA / `Total Enrollment`)) %>% 
  mutate(pct_asian = (Asian / `Total Enrollment`)) %>% 
  mutate(pct_black = (Black_or_AA / `Total Enrollment`)) %>% 
  mutate(pct_nwopi = (NW_OPI / `Total Enrollment`)) %>% 
  mutate(pct_white = (White / `Total Enrollment`)) %>% 
  mutate(pct_2more = (`Two or more races`/ `Total Enrollment`)) %>% 
  mutate(d_index = (1 - pct_hisp^2 - pct_ai_na^2 - pct_asian^2 - 
                      pct_black^2 - pct_nwopi^2 - pct_white^2 - pct_2more^2)) %>% 
  mutate(metric = SUM/13)



# Dataset Complete ----------------
df
df %>% 
  describe()
write_excel_csv(df, "analytic_data.csv")

# Begin Plots for EDA ----------------------------

df %>% 
  ggplot(aes(x = `Census Total`, y = AB_IB_Dual_Enrollment, color = urbanicity)) + 
  geom_point()  + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(
    title = "Participation in College Readiness Courses by Population and Urbanicity",
    x = "log of census population (2010)",
    y = "enrollment in college readiness courses", 
    color = "urbanicity",
    caption = "College Readiness courses are AP, IB, or Dual-Enrollment courses"
  ) + 
  theme_textbook() + 
  scale_color_manual(values = uvapal)


fonts()


model1<-lm(AB_IB_Dual_Enrollment ~ census + mostly + urban, data = df)
model2<-lm(AB_IB_Dual_Enrollment ~ log(census) + mostly + urban, data = df) #need log of census as offset variable? 
model3<-lm(AB_IB_Dual_Enrollment ~ log(census) + mostly + urban + `Total FRPL`, data = df)

stata_summary(model1)
stata_summary(model2)
summary(model3)

df %>% 
  dplyr::select(Name, metric) %>% 
  arrange(-metric)


############################################################
names(df)
model3<-lm(AB_IB_Dual_Enrollment ~ log(census) + mostly + urban + `Total FRPL`, data = df)




df %>% 
  ggplot(aes(x = pct_frpl, y = pct_apib, color = urbanicity)) +
  geom_point() +
  #scale_x_log10() +
  #scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(
    title = "Participation in College Readiness Courses by FRPL and Urbanicity",
    x = "percent FRPL",
    y = "enrollment in college readiness courses", 
    color = "urbanicity",
    caption = "College Readiness courses are AP, IB, or Dual-Enrollment courses"
  ) + 
  theme_textbook() + 
  scale_color_manual(values = uvapal)


##############################################################################
names(df)
hist(df$pct_adv)
describe(df$pct_adv)
print(nrow(df))


df %>% 
  ggplot(aes(y = pct_adv, x = pct_frpl, color = urbanicity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Percent of Students with Advanced Diplomas By Poverty and Urbanicity", 
    x = "percent FRPL", 
    y = "Percent Advanced Studies Diploma"
  ) +
  theme_solarized() +
  scale_color_solarized()



############################################################################


# lm(metric ~ urban + mostly + pct_frpl + pct_white + log(`Total Enrollment`), data = df) %>% 
#   stata_summary()


lm(metric ~ urban + mostly + pct_frpl + pct_white + log(`Total Enrollment`) + pct_frpl*pct_white, data = df) %>% 
  stata_summary()



describe(df$pct_frpl)
describe(df$pct_white)
frpldif<-(100-6.16)*(-0.0078968)
whitedif<-(98.98-2.46)*(-0.0052648)
print(frpldif)
whitedif




# Calculate diveristy Index ---

# D_sub_i = 1 - p^2k -P^2k2 etc = 1 - summa(p^2)

names(df)


hist(df$d_index)





# re-run models with diversity score --------------------------------

lm(metric ~ urban + mostly + pct_frpl + d_index + log(`Total Enrollment`), data = df) %>% 
  stata_summary()

df %>% 
  lm(pct_adv ~ urban + mostly + pct_frpl + d_index + log(`Total Enrollment`), data = .) %>% 
  stata_summary()

model5<-df %>% 
  lm(pct_adv~ pct_frpl + d_index + log(`Total Enrollment`), data = .)
summary(model5)
stata_summary(model5)

  # OKay, we need to include and interpret model 5 because wtf. 
  # Well. Model 5 with clustering when we do clustering below. 


model6<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`), data = df)
model9<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + urban + mostly, data = df)
model7<- lm(metric ~ pct_frpl  + log(`Total Enrollment`), data = df)
summary(model6)
sum(is.na(df$`Total Enrollment`))

df %>% 
  dplyr::select(`Total Enrollment`, pct_frpl, metric) %>% 
  View()

names(df)
df %>% 
  filter(!is.na(metric)) %>% 
  mutate(pts = fitted(model7, level = 0, asList = FALSE)) %>% 
  ggplot(aes(y = pts, x = d_index)) +
  # geom_point(# aes(color = factor(`Division num`)),# 
  #  color = "#E57200", alpha = 0.8, stroke = 0, size = 2) +
  geom_text(alpha = 0.8, aes(label = Name)) + 
  geom_smooth(color = "#232D4B", method = "lm") + 
  labs(
    title = "Levels of Courses and Diversity", 
    subtitle = "Controlling for FRPL and log of enrollment",
    x = "Diversity - Index (Kelly & Price, 2011)",
    y = "Restricted Model Predicted Course Levels"
  ) + 
  theme_textbook()
  

model8<-lm(metric~d_index, data = df)
mccrr::gg_added_var(partial = model8, extended = model7)
rockchalk::outreg(list(
  "electivity" = model6, 
  "electivity" = model9
), type = "html")
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




# diploma status ----------------------------------
# 

model0b <- df %>% 
  lmer(pct_adv_dip~ 1 + (1 | `Division num`), data = .)
icc(model0b)
model17 <- df %>% 
  lmer(pct_adv_dip~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + urban + mostly + (1 | `Division num`), data = .)
model18<- df %>% 
  lmer(pct_adv_dip~ 1 + d_index  + pct_frpl + log(`Total Enrollment`)  + (1 | `Division num`), data = .)
model19<- df %>% 
  lmer(pct_adv_dip~ 1 + d_index * pct_frpl + log(`Total Enrollment`)  + (1 | `Division num`), data = .)

rockchalk::outreg(
  list(
    "Var Comps" = model0b,
    "Levels"    = model17, 
    "Levels"    = model18,
    "Levels"    = model19
  ), 
  type = "html")



# Clustered Standard Errors Models:
names(df)
library(multiwayvcov)
library(miceadds)
dfclust<- df %>% 
  rename(division = `Division num`) %>% 
  rename(enrollment = `Total Enrollment`) %>% 
  rename(pct_rural = `Census Percent Rural`) %>% 
  dplyr::select(metric, d_index, pct_frpl, enrollment, division, urban, mostly, rural, pct_rural)
dfclust  


clust1<- lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(enrollment), 
             cluster = dfclust$division)
summary(clust1)
clust2<-lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(enrollment) 
                   + urban + rural, 
                   cluster = dfclust$division)
clust3<-lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(enrollment) + pct_rural,
                   cluster = dfclust$division) # Irrelevant.
#tab_model(clust1, clust2)
summary(clust2)
summary(clust3)
# fr*ck this I'm doing it by hand.


rockchalk::outreg(
  list(
    "Electivity" = clust1,
    "Electivity" = clust2
  )
)
tab_model(model6, model9)

# do this again with pmin 

regress(pct_advanced ~ pmin*metric + pct_frpl + log(`Total Enrollment`))


library(ggthemes)
names(df)
library(university)
### ANOVA
# is the difference in leveledness of subject significant?
pos<- read_csv("C:\\Users\\Andrew\\Desktop\\va_tracking-master\\data_files\\pos_clean_3.csv")

pos %>% 
  dplyr::select(num_sci, num_eng, num_math, num_hist) %>% 
  rename(Science = num_sci, 
         English = num_eng, 
         Math = num_math, 
         History = num_hist) %>% 
  reshape2::melt() %>% 
  ggplot(aes(y = value, x = variable)) +
  # geom_violin() +
  geom_boxplot() +
  geom_jitter(alpha = 0.25) +
  # scale_color_uva() +
  theme_textbook() +
  labs(
    title = "Number of Tracks by Subject",
    y = "Tracks",
    x = "Subject", 
    color = "Subject"
  ) 
names(pos)


# this model is wrong. does not account for nesting. 
subj_model_1 <- pos %>% 
  rename(id = dist_id) %>% 
  dplyr::select(id, num_sci, num_eng, num_math, num_hist) %>%
  rename(Science = num_sci, 
         English = num_eng, 
         Math = num_math, 
         History = num_hist) %>% 
  reshape2::melt(id = "id") %>% 
  lm(value~variable, data = .)
anova(subj_model_1)
stata_summary(subj_model_1)

# this model is wrong. does not account for nesting. 
pos %>% 
  dplyr::select(id, num_sci, num_eng, num_math, num_hist) %>%
  rename(Science = num_sci, 
         English = num_eng, 
         Math = num_math, 
         History = num_hist) %>%  
  reshape2::melt(id = "id") %>% 
  reshape2::dcast(id ~ variable, mean )  

library(nlme)
library(car)
subj_model_2_data <- pos %>% 
  rename(id = dist_id) %>%  
  dplyr::select(id, num_sci, num_eng, num_math, num_hist) %>%
  rename(Science = num_sci, 
         English = num_eng, 
         Math = num_math, 
         History = num_hist) %>% 
  reshape2::melt(id = "id") 

summary(
  aov(value ~ variable + Error(id/variable), data = subj_model_2_data)
)
subj_m2<-aov(value ~ variable + Error(id/variable), data = subj_model_2_data)
summary(subj_m2)
pos %>% 
  dplyr::select(dist_id, num_sci, num_eng, num_math, num_hist) %>%
  psych::describe()



# other things:
df3 <- df%>% 
  select(metric, d_index) %>% 
  filter(metric !=0)
df3 %>% 
  describe()
df %>% 
  arrange(d_index) %>% 
  select(d_index)
df %>% 
  filter(id == 23 | id == 75) %>%  #14
  select(id, Name, metric, d_index)

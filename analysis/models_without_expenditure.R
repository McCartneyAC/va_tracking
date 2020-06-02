library(multiwayvcov)
library(miceadds)

df<-read_csv("analytic_data.csv")

# Top scorer for trackedness:
df %>% 
  dplyr::select(Name, metric) %>% 
  arrange(-metric)


# initial naive look

lm(metric ~ urban + mostly + pct_frpl + 
     pct_white + log(census) + 
     pct_frpl*pct_white, data = df) %>% 
  summary()

# re-run models with diversity score --------------------------------

lm(metric ~ urban + mostly + pct_frpl + d_index + log(census), data = df) %>% 
  summary()

df %>% 
  lm(pct_adv ~ urban + mostly + pct_frpl + d_index + log(census), data = .) %>% 
  summary()

model5<-df %>% 
  lm(pct_adv~ pct_frpl + d_index + 
       log(census), data = .)
summary(model5)



#models without accounting for clustering
model6<- lm(metric ~ d_index + pct_frpl + log(census), data = df)
model9<- lm(metric ~ d_index + pct_frpl + log(census) + 
              urban + mostly, data = df)


# restricted model
model7<- lm(metric ~ pct_frpl  + log(census), data = df)
model7 %>% summary()


# MAIN GRAPHIC OUTPUT (no clustering)
df %>% 
  filter(!is.na(metric)) %>%
  filter(!is.na(census)) %>% 
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


# pure effect: -----------------------
model8<-lm(metric~d_index, data = df)
model8 %>% summary()



# lme models -----------------------------
model0 <- df %>% 
  lmer(metric~ 1 + (1 | `Division num`), data = .)
icc(model0)
model15 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + urban + mostly + (1 | `Division num`), data = .)
model16 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + (1 | `Division num`), data = .)




# Final Prferred models (not clustered) ---------------------------
model6<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`), data = df)
model9<- lm(metric ~ d_index + pct_frpl + log(`Total Enrollment`) + urban + mostly, data = df)
model15 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + urban + mostly + (1 | `Division num`), data = .)
model16 <- df %>% 
  lmer(metric~ 1 + d_index  + pct_frpl + log(`Total Enrollment`) + (1 | `Division num`), data = .)


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





names(df)


# Clustered Standard Errors Models: ###############################################


dfclust<- df %>% 
  dplyr::select(metric, d_index, pct_frpl, census, division, urban, mostly, rural, pct_rural)
dfclust  


clust1<- lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(census), 
                    cluster = dfclust$division)
summary(clust1)
clust2<-lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(census) 
                   + urban + rural, 
                   cluster = dfclust$division)
clust3<-lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(census) + pct_rural,
                   cluster = dfclust$division) 
# Irrelevant.
tab_model(clust1, clust2)


summary(clust2)
summary(clust3)

rockchalk::outreg(
  list(
    "Electivity" = clust1,
    "Electivity" = clust2
  )
)


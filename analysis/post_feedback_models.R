library(multiwayvcov)
library(miceadds)

df<-read_csv("analytic_data.csv")
df

# Clustered Standard Errors Models: ###############################################


dfclust<- df %>% 
  dplyr::select(metric, d_index, pct_frpl, 
                enrollment , division, urban, mostly, rural, 
                pct_rural, Expenditure_per_pupil )
dfclust  


clust1<- lm.cluster(data = dfclust, 
                    metric ~ d_index + pct_frpl + log(enrollment) + 
                      Expenditure_per_pupil ,
                    cluster = dfclust$division)
summary(clust1)
clust2<-lm.cluster(data = dfclust, 
                   metric ~ d_index + pct_frpl + log(enrollment) +
                     Expenditure_per_pupil + urban + rural, 
                   cluster = dfclust$division)
clust3<-lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(enrollment) + 
                     Expenditure_per_pupil +  pct_rural,
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









clust1

# restricted model
clust_null<- lm.cluster(data = dfclust, 
                        metric ~  pct_frpl + log(enrollment) + 
                          Expenditure_per_pupil ,
                        cluster = dfclust$division)






  

  # ########################### #
  # Trying again with estimatr  # 
  # ########################### #
  
install.packages("estimatr")
library(estimatr)  

# example
res_cl <- lm_robust(y ~ z, data = dat, clusters = clusterID)

main_model<- lm_robust(metric ~ d_index + pct_frpl + log(enrollment) + 
                      Expenditure_per_pupil ,
                    data = dfclust, 
                    clusters = division)
summary(main_model)
mod_s_expenditure<-lm_robust(metric ~ d_index + pct_frpl + log(enrollment),
                             data = dfclust, 
                             clusters = division)



restricted_main<-lm_robust(metric ~  pct_frpl + log(enrollment) + 
                             Expenditure_per_pupil ,
                           data = dfclust, 
                           clusters = division)

# MAIN GRAPHIC OUTPUT (no clustering)
df %>% 
  filter(!is.na(metric)) %>%
  filter(!is.na(enrollment)) %>% 
  mutate(pts = fitted(restricted_main, level = 0, asList = FALSE)) %>% 
  ggplot(aes(y = pts, x = d_index)) +
  # geom_point(# aes(color = factor(`Division num`)),# 
  #  color = "#E57200", alpha = 0.8, stroke = 0, size = 2) +
  geom_text(alpha = 0.8, aes(label = Name)) + 
  geom_smooth(color = "#232D4B", method = "lm") + 
  labs(
    title = "Levels of Courses and Diversity", 
    subtitle = "Controlling for FRPL, enrollment, and expenditure",
    x = "Diversity - Index (Kelly & Price, 2011)",
    y = "Restricted Model Predicted Course Levels"
  ) + 
  theme_textbook()




tab_model(main_model,mod_s_expenditure)

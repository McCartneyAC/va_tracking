library(multiwayvcov)
library(miceadds)
use <- function(name) {
  # consider future support for .json? 
  if (grepl(".csv", name)) {
    readr::read_csv(name)
  } else if (grepl(".xlsx", name)) {
    readxl::read_xlsx(name)
  } else if (grepl(".dta", name)) {
    haven::read_dta(name)
  } else if (grepl( ".sav", name)) {
    haven::read_spss(name)
  } else if (grep1(".rda", name)) {
    load(name)
  } else {
    stop("unknown data type.")
  }
}

df<-use("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\analytic_data.csv")
df

# Clustered Standard Errors Models: ###############################################


dfclust<- df %>% 
  dplyr::select(metric, d_index, pct_frpl, 
                census , division, urban, mostly, rural, 
                pct_rural, Expenditure_per_pupil )
dfclust  


clust1<- lm.cluster(data = dfclust, 
                    metric ~ d_index + pct_frpl + log(census) + 
                      Expenditure_per_pupil ,
                    cluster = dfclust$division)
summary(clust1)
clust2<-lm.cluster(data = dfclust, 
                   metric ~ d_index + pct_frpl + log(census) +
                     Expenditure_per_pupil + urban + rural, 
                   cluster = dfclust$division)
clust3<-lm.cluster(data = dfclust, metric ~ d_index + pct_frpl + log(census) + 
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

main_model<- lm_robust(metric ~ d_index + pct_frpl + log(census) + 
                      Expenditure_per_pupil ,
                    data = dfclust, 
                    clusters = division)
summary(main_model)
mod_s_expenditure<-lm_robust(metric ~ d_index + pct_frpl + log(census),
                             data = dfclust, 
                             clusters = division)
exp_c_urbanicity <- lm_robust(metric ~ d_index + pct_frpl + log(census) + 
                                Expenditure_per_pupil:pct_rural ,
                              data = dfclust, 
                              clusters = division)



describe(dfclust, fast = T)
tab_model(main_model, mod_s_expenditure, model_3, exp_c_urbanicity
          )





model_3<- lm_robust(metric ~ d_index  + log(census) + 
                        Expenditure_per_pupil ,
                      data = dfclust, 
                      clusters = division)
div_exp<-lm_robust(Expenditure_per_pupil ~pct_frpl ,
                   data = dfclust, 
                   clusters = division)
met_exp<-lm_robust(metric ~Expenditure_per_pupil ,
                   data = dfclust, 
                   clusters = division)
tab_model(div_exp, met_exp)
# MAIN GRAPHIC OUTPUT (no clustering)
df %>% 
  filter(!is.na(metric)) %>%
  filter(!is.na(census)) %>% 
  mutate(pts = fitted(restricted_main, level = 0, asList = FALSE)) %>% 
  ggplot(aes(y = pts, x = d_index)) +
  # geom_point(# aes(color = factor(`Division num`)),# 
  #  color = "#E57200", alpha = 0.8, stroke = 0, size = 2) +
  geom_text(alpha = 0.8, aes(label = Name)) + 
  geom_smooth(color = "#232D4B", method = "lm_robust") + 
  labs(
    title = "Levels of Courses and Diversity", 
    subtitle = "Controlling for FRPL, enrollment, and expenditure",
    x = "Diversity - Index (Kelly & Price, 2011)",
    y = "Restricted Model Predicted Course Levels"
  ) + 
  theme_textbook()




tab_model(main_model,mod_s_expenditure)



##########################
df %>% 
  ggplot(aes(x = fct_reorder(Name, -Expenditure_per_pupil), y = Expenditure_per_pupil)) + 
  geom_col() +
    coord_flip()
  
df %>% 
  ggplot(aes(x = Expenditure_per_pupil)) + 
  geom_histogram() + 
  scale_x_continuous(breaks =seq(from = 9000, to = 21000, by = 1000))
?seq

df %>% 
  arrange(-Expenditure_per_pupil) %>% 
  dplyr::select(Name, Expenditure_per_pupil)





# PUblishing These Models God Damn ----------------------------------------


main_model<- lm_robust(metric ~ d_index + pct_frpl + log10(census), data = dfclust, 
                       clusters = factor(division))

main_with_urban<-lm_robust(metric ~ d_index + pct_frpl + log10(census) + pct_rural,
                             data = dfclust, 
                             clusters = division)


tab_model(main_model, show.se = TRUE)
#WHAT THE FUCK

df

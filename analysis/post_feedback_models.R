library(multiwayvcov)
library(miceadds)


df<-use("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\analytic_data.csv")
df

# Clustered Standard Errors Models: ###############################################


dfclust<- df %>% 
  dplyr::select(metric, d_index, pct_frpl, 
                census , division, urban, mostly, rural, 
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

main_model<- lm_robust(metric ~ d_index + pct_frpl + log(enrollment) + 
                      Expenditure_per_pupil ,
                    data = dfclust, 
                    clusters = division)
summary(main_model)
mod_s_expenditure<-lm_robust(metric ~ d_index + pct_frpl + log(enrollment),
                             data = dfclust, 
                             clusters = division)
exp_c_urbanicity <- lm_robust(metric ~ d_index + pct_frpl + log(enrollment) + 
                                Expenditure_per_pupil:pct_rural ,
                              data = dfclust, 
                              clusters = division)



describe(dfclust, fast = T)
tab_model(main_model, mod_s_expenditure, model_3, exp_c_urbanicity
          )





model_3<- lm_robust(metric ~ d_index  + log(enrollment) + 
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




library(viridis)
library(tidyverse)
library(cowplot)     # for theme_map()
library(colorspace)  # for scale_fill_continuous_sequential()
library(sf)       
getwd()
shapes<-sf::st_read(system.file("C://Users//Andrew//Desktop//Statistics and Data Analysis//va_tracking-master//data_files//", package = "sf"))
va <- sf::st_read(system.file("shape/tl_2016_51_cousub.shp", package = "sf"), quiet = TRUE)

va <- sf::st_read(
"C://Users//Andrew//Desktop//Statistics and Data Analysis//va_tracking-master//data_files//admin_shapefiles//VirginiaCounty.shp")
plot(va)
df
head(va)
va <- va %>% 
  mutate(Name = NAMELSAD )
va <- va %>% 
  left_join(df, by = "Name")
va

# GO TO EXCEL. FIND AND REPLACE city WITH City 
ggplot(va, aes(fill = d_index )) + 
  geom_sf(color = "white") +
  scale_fill_continuous_sequential(
    palette = "Blues", rev = TRUE,
    na.value = "grey60",
    name = "D-Index (Kelly, 1999)",
    #limits = c(9000, 21000),
   # breaks = 3000*c(1:5),
   # labels = c("$9,000", "$12,000", "$15,000", "$18,000", "$21,000")
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      barwidth = grid::unit(3.0, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) + 
  theme_map(12) +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.02, 0.8)
  )




df %>% 
  mccrr::dossier(Name, "Fairfax County")
df %>% 
  arrange(-d_index) %>% 
  dplyr::select(Name, d_index)

library(ggthemes)
names(df)
library(university)
### ANOVA
# is the difference in leveledness of subject significant?
pos<- read_csv("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\pos_clean_3.csv")

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


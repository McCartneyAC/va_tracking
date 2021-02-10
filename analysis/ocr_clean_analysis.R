# ocr analysis cleaner. 

setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr_va\\")

data_long<- read_csv("ocr_merge_0104_long.csv")
data_medium<-read_csv("data_medium.csv")


malg2 <- data_medium %>% 
  filter(course == "alg2")  %>% 
  group_by(id) %>% 
  center(schpctwhite) %>% 
  ungroup() %>% 
  center(trackedness) %>% 
  center(d_index) %>% 
  standardize(schoolsum) %>% 
  lmer(discrepancy_black  ~1 +  schpctwhite+ schoolsum +trackedness+  d_index +   log10(census)+(1 | id) , data = .) 



mapany <- data_medium %>% 
  filter(course == "apany" )  %>% 
  group_by(id) %>% 
  center(schpctwhite) %>% 
  ungroup() %>% 
  center(trackedness) %>% 
  center(d_index) %>% 
  standardize(schoolsum) %>% 
  lmer(discrepancy_black  ~1 +  schpctwhite+ schoolsum +  trackedness +   d_index + log10(census)+(1 | id) , data = .) 


mbio <- data_medium %>% 
  filter(course == "bio" )  %>% 
  group_by(id) %>% 
  center(schpctwhite) %>% 
  ungroup() %>% 
  center(trackedness) %>% 
  center(d_index) %>% 
  standardize(schoolsum) %>% 
  lmer(discrepancy_black  ~1 + schpctwhite+  schoolsum + trackedness +  d_index +  log10(census)+(1 | id) , data = .)


mchem <- data_medium %>% 
  filter(course == "chem")  %>% 
  group_by(id) %>% 
  center(schpctwhite) %>% 
  ungroup() %>% 
  center(trackedness) %>% 
  center(d_index) %>% 
  standardize(schoolsum) %>% 
  lmer(discrepancy_black  ~1 +  schpctwhite+  schoolsum +trackedness +   d_index +log10(census)+(1 | id) , data = .) 


# Final Preferred Models --------------------------------------------------


dvs<-c(
  # "Algebra 1 in 11-12"  ,
  # "Algebra 1 in 8th",
  # "Algebra 1 in 9-10",
  "Algebra 2"      ,
  "AP Class"    ,
  # "AP Test",
  "Biology"     ,
  "Chemistry"  #  ,
  #"Gifted"    
)
sjPlot::tab_model( c( 
  # malg11112,  
  # malg18,   
  # malg1910,  
  malg2,   mapany, 
  #maponemore,  
  mbio,mchem#,  
  #mgifted
),   dv.labels = dvs, 
show.se = TRUE
)





# round two ---------------------------------------------------------------



bf_map<-data_medium %>% 
  filter(course == "apany" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  mutate(slots = 100*(schoolsum/schpop)) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness +  d_index + 
          + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
          (1 | id) , data = .)  




bf_alg2<-data_medium %>% 
  filter(course == "alg2" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  rename(slots = schoolsum) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness +  d_index + 
          + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
          (1 | id) , data = .)  
bf_chem<-data_medium %>% 
  filter(course == "chem" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  rename(slots = schoolsum) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness +  d_index + 
          + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
          (1 | id) , data = .)  
bf_bio<-data_medium %>% 
  filter(course == "bio" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  rename(slots = schoolsum) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness +  d_index + 
        + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
          (1 | id) , data = .)  
data_medium

sjPlot::tab_model( c( 
  # malg11112,  
  # malg18,   
  # malg1910,  
  bf_alg2,   
  bf_map, 
  #maponemore,  
  bf_bio,
  bf_chem#,  
  #mgifted
),   dv.labels = dvs, 
show.se = TRUE
)

bf_chem

report(bf_map)
report(bf_chem)

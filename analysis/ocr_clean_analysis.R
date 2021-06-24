# ocr analysis cleaner. 

setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr_va\\")

data_long<- read_csv("ocr_merge_06232021_long.csv")
#data_medium<-read_csv("data_medium.csv")
data_medium<-read_csv("data_medium_06232021.csv")
data_medium %>% 
  dplyr::select(School, course, dindex, d_index, sch_dindex, schpop, id)  %>% 
  head(20)
data_long


zscale <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


# dataset updates ---------------------------------------------------------


data_medium

data_medium <- data_medium %>% 
  mutate(rcdi_black = ((pctblack-schpctblack)/schpctblack)*100) %>% 
  mutate(rcdi_latinx = ((pctlatinx-schpctlatinx)/schpctlatinx)*100) 
data_medium %>% 
  select(School, SCHID, course, pctlatinx, schpctlatinx, rcdi_latinx) %>% 
  filter(course =="alg10910") %>% 
  view()

# Prerequisites
# prereq<-mccrr::paste_data()
prereq<-read_csv("prerequisites_by_distrct.csv")
# prereq <- prereq %>%
#   select(Name, dist_id, ratio) %>%
#   rename(id = dist_id) %>%
#   rename(prereq_ratio = ratio)
# prereq <- prereq %>%
#   select(id, prereq_ratio)
# 
# write_csv(prereq, "prerequisites_by_distrct.csv")

data_medium <- data_medium %>% 
  left_join(prereq, by = "id") 
# 
# data_medium %>% 
#   filter(id %in% c(48, 73, 81)) %>% view()
# 


# malg2 <- data_medium %>% 
#   filter(course == "alg2")  %>% 
#   group_by(id) %>% 
#   center(schpctwhite) %>% 
#   ungroup() %>% 
#   center(trackedness) %>% 
#   center(d_index) %>% 
#   standardize(schoolsum) %>% 
#   lmer(discrepancy_black  ~1 +  schpctwhite+ schoolsum +trackedness+  d_index +   log10(census)+(1 | id) , data = .) 



# mapany <- data_medium %>% 
#   filter(course == "apany" )  %>% 
#   group_by(id) %>% 
#   center(schpctwhite) %>% 
#   ungroup() %>% 
#   center(trackedness) %>% 
#   center(d_index) %>% 
#   standardize(schoolsum) %>% 
#   lmer(discrepancy_black  ~1 +  schpctwhite+ schoolsum +  trackedness +   d_index + log10(census)+(1 | id) , data = .) 
# 
# 
# mbio <- data_medium %>% 
#   filter(course == "bio" )  %>% 
#   group_by(id) %>% 
#   center(schpctwhite) %>% 
#   ungroup() %>% 
#   center(trackedness) %>% 
#   center(d_index) %>% 
#   standardize(schoolsum) %>% 
#   lmer(discrepancy_black  ~1 + schpctwhite+  schoolsum + trackedness +  d_index +  log10(census)+(1 | id) , data = .)
# 
# 
# mchem <- data_medium %>% 
#   filter(course == "chem")  %>% 
#   group_by(id) %>% 
#   center(schpctwhite) %>% 
#   ungroup() %>% 
#   center(trackedness) %>% 
#   center(d_index) %>% 
#   standardize(schoolsum) %>% 
#   lmer(discrepancy_black  ~1 +  schpctwhite+  schoolsum +trackedness +   d_index +log10(census)+(1 | id) , data = .) 
# 

# Final Preferred Models --------------------------------------------------

# 
# dvs<-c(
#   # "Algebra 1 in 11-12"  ,
#   # "Algebra 1 in 8th",
#   # "Algebra 1 in 9-10",
#   "Algebra 2"      ,
#   "AP Class"    ,
#   # "AP Test",
#   "Biology"     ,
#   "Chemistry"  #  ,
#   #"Gifted"    
# )
# sjPlot::tab_model( c( 
#   # malg11112,  
#   # malg18,   
#   # malg1910,  
#   malg2,   mapany, 
#   #maponemore,  
#   mbio,mchem#,  
#   #mgifted
# ),   dv.labels = dvs, 
# show.se = TRUE
# )
# 
# 



# round two ---------------------------------------------------------------


# 
# bf_map<-data_medium %>% 
#   filter(course == "apany" )  %>% 
#   mutate(schpctwhite = (schpctwhite*100)) %>% 
#   mutate(slots = 100*(schoolsum/schpop)) %>% 
#   mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
#   lmer( discrepancy_black ~1 + trackedness +  d_index + 
#           + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
#           (1 | id) , data = .)  



# 
# bf_alg2<-data_medium %>% 
#   filter(course == "alg2" )  %>% 
#   mutate(schpctwhite = (schpctwhite*100)) %>% 
#   rename(slots = schoolsum) %>% 
#   mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
#   lmer( discrepancy_black ~1 + trackedness +  d_index + 
#           + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
#           (1 | id) , data = .)  
# bf_chem<-data_medium %>% 
#   filter(course == "chem" )  %>% 
#   mutate(schpctwhite = (schpctwhite*100)) %>% 
#   rename(slots = schoolsum) %>% 
#   mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
#   lmer( discrepancy_black ~1 + trackedness +  d_index + 
#           + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
#           (1 | id) , data = .)  
# bf_bio<-data_medium %>% 
#   filter(course == "bio" )  %>% 
#   mutate(schpctwhite = (schpctwhite*100)) %>% 
#   rename(slots = schoolsum) %>% 
#   mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
#   lmer( discrepancy_black ~1 + trackedness +  d_index + 
#         + Expenditure_per_pupil+  log10(schpop) + pct_frpl + 
#           (1 | id) , data = .)  
# data_medium
# 
# sjPlot::tab_model( c( 
#   # malg11112,  
#   # malg18,   
#   # malg1910,  
#   bf_alg2,   
#   bf_map, 
#   #maponemore,  
#   bf_bio,
#   bf_chem#,  
#   #mgifted
# ),   dv.labels = dvs, 
# show.se = TRUE
# )
# 
# bf_chem
# 
# report(bf_map)
# report(bf_chem)
# 



# may 26 update -----------------------------------------------------------

# 
# # examine outcome
# data_medium %>% 
#   filter(course %in% c("chem", "apany")) %>% 
#   select(SCHID, course, rcdi_black, rcdi_latinx) %>% 
#   pivot_longer(cols = starts_with("rcdi")) %>% 
#   ggplot(aes(x = value, y = name, fill = name)) +
#   geom_violin() +
#   theme_light() + 
#   facet_wrap(~course)
# 
# 
# 
# data_medium %>% 
#   filter(course %in% c("chem", "apany")) %>% 
#   select(School, SCHID, course, pctblack, schpctblack, rcdi_black) %>% 
#   ggplot(aes(x=schpctblack, y = pctblack, size = abs(rcdi_black))) +
#   geom_point(alpha = 0.6) +
#   geom_abline(slope = 1, intercept = 0)+
#   geom_smooth()+
#   facet_wrap(~course)
# 
# # does overrepresentation in alg1 in 9-10 relate to underrepresentation in chem
# data_medium  %>% 
#   filter(course %in% c("chem", "alg11112")) %>% 
#   select(School, SCHID, course, pctblack, schpctblack, rcdi_black) %>% 
#   pivot_wider(id_cols = c(School, SCHID), names_from = course, values_from = rcdi_black) 
#   ggplot(aes(x=alg10910, y = chem)) +
#   geom_point(alpha = 0.6) +
#   geom_abline(slope = 1, intercept = 0)+
#   geom_smooth() + 
#   geom_smooth(method = "lm", color = "orange") +
#   theme_light()
#   
#   ?pivot_wider
# data_medium  %>% 
#   filter(course %in% c("chem", "alg10910")) %>% 
#   select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id) %>% 
#   pivot_wider(id_cols = c(School, SCHID, id), names_from = course, values_from = rcdi_black) %>% 
#   lmer(chem~1 + alg10910 + (1|id), data = .) %>% 
#   tab_model()
# edlf8360
# # first real model?
data_medium %>%
  filter(course %in% c("chem", "alg10910")) %>%
  filter(id != 924) %>%
  select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index),
              names_from = course, values_from = rcdi_black) %>%
  mutate(zd_index = zscale(d_index)) %>%
  mutate(zcensus= zscale(census)) %>%
  mutate(zpct_frpl=zscale(pct_frpl)) %>%
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>%
  mutate(zsch_dindex = zscale(sch_dindex)) %>%
  mutate(zschpop = zscale(schpop)) %>%
  mutate(zalg10910 = zscale(alg10910)) %>%
  mutate(zchem = zscale(chem)) %>%
  ungroup() %>%
  lmer(chem~1+zalg10910 + zsch_dindex +  zschpop +zcensus + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .) %>%
  sjPlot::tab_model()
# 
# # RDCI ranges
# data_medium %>% 
# filter(course %in% c("chem", "alg10910")) %>% 
#   filter(id != 924) %>% 
#   select(School, SCHID, course, pctblack, schpctblack, rcdi_black , id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index) %>%
#   pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index), 
#               names_from = course, values_from = rcdi_black ) %>% 
#   select(School, chem) %>% 
#   ggplot(aes(x = chem)) + 
#   geom_histogram()




# checking RDCI math ------------------------------------------------------


# # who has a high rcdi??? 
# data_medium %>% 
#   filter(course %in% c("chem", "apany")) %>% 
#   select(School, id, SCHID, course, pctblack, schpctblack, rcdi_black) %>% 
#   arrange(desc(rcdi_black)) %>% 
#   head(20) %>% 
#   rename(District_ID = id) %>% 
#   gt()
# data_medium %>% 
#   filter(School == "Central High") %>% 
#   select(School, id, SCHID, course, pctblack, schpctblack, rcdi_black, schpop) %>% 
#   view()



# using logs? -------------------------------------------------------------

# data_medium %>% 
#   select(School, SCHID, id, LEA, census, schpop) %>% 
#   distinct() %>% 
#   # mutate(logpop = log10(schpop+1)) %>% 
#   # pivot_longer(cols = c(schpop, logpop)) 
#   ggplot(aes(x = schpop) ) +
#   geom_density(fill="red") -> p1
# 
# 
# data_medium %>% 
#   select(School, SCHID, id, LEA, census, schpop) %>% 
#   distinct() %>% 
#   mutate(logpop = log10(schpop+1)) %>% 
#   ggplot(aes(x = logpop)  ) +
#   geom_density(fill="blue") -> p2
# 
# p1 + p2
# 
# 
# 
# data_medium %>% 
#   select(id, LEA, census) %>% 
#   distinct() %>% 
#   ggplot(aes(x = census) ) +
#   geom_density(fill="purple") -> p3
# 
# data_medium %>% 
#   select(id, LEA, census) %>% 
#   distinct() %>% 
#   mutate(logcensus = log10(census+1)) %>% 
#   ggplot(aes(x = logcensus)  ) +
#   geom_density(fill="green") -> p4
# 
# p3+p4


# YES WE ARE GOING WITH LOGARITHMS



# Intra-Cluster Correlations ----------------------------------------------

data_medium %>% 
  filter(course %in% c("chem", "alg10910")) %>% 
  filter(id != 924) %>% # these are JDCs
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index), 
              names_from = course, values_from = rcdi_latinx) %>% 
  mutate(zd_index = zscale(d_index)) %>%
  mutate(zcensus= zscale(census)) %>%
  mutate(zpct_frpl=zscale(pct_frpl)) %>%
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>%
  mutate(zsch_dindex = zscale(sch_dindex)) %>%
  mutate(zschpop = zscale(schpop)) %>%
  mutate(zalg10910 = zscale(alg10910)) %>%
  mutate(zchem = zscale(chem)) %>%
  ungroup() %>%
  lmer(chem~1+  (1 |id), data = .) %>% 
  sjPlot::tab_model()



# prereqs model -----------------------------------------------------------

data_medium %>% 
  filter(course %in% c("chem", "alg10910")) %>% 
  filter(id != 924) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio), 
              names_from = course, values_from = rcdi_black) %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  mutate(zprereq = zscale(prereq_ratio)) %>% 
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zalg10910 = zscale(alg10910)) %>% 
  mutate(zchem = zscale(chem)) %>% 
  ungroup() %>% 
  select(chem, zalg10910, zsch_dindex, log_schpop, log_census, zpct_frpl, zExpenditure_per_pupil, zd_index, id, zprereq) %>% 
  lmer(chem~1+ zprereq + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .) %>% 
  sjPlot::tab_model()

data_medium %>% 
  filter(course =="apany") %>% 
  filter(id != 924) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio), 
              names_from = course, values_from = rcdi_black)  %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  mutate(zprereq = zscale(prereq_ratio)) %>% 
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  ungroup() %>% 
  select(apany,  zsch_dindex, log_schpop, log_census, zpct_frpl, zExpenditure_per_pupil, zd_index, id, zprereq) %>% 
  lmer(apany~1+ zprereq + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .) 



# AP Indicator ------------------------------------------------------------
ap_count<-read_csv("ap_count.csv")
data_medium %>% 
  filter(course == "apany")
ap_count <- ap_count %>% 
  select(SCHID, SCH_APCOURSES) %>% 
  rename(apcount = SCH_APCOURSES)
ap_count
# data_medium %>% 
#   left_join(ap_count, by = "SCHID") %>% 
#   mutate(APSAI = (schpop/apcount)) %>% 
#   select(School, SCHID, LEA, apcount, schpop, APSAI) %>% 
#   arrange((APSAI)) %>% 
#   distinct(School, SCHID, LEA, apcount, schpop, APSAI) %>% 
#   write_csv("suspicions.csv")
# IB_list <- c(7,
#              21,
#              23,
#              29,
#              109,
#              42,
#              43,
#              117,
#              74,
#              75,
#              123,
#              139,
#              88,
#              89,
#              127,
#              128,
#              98
# )
IB_schools<-c(
'01280',
'00112',
'00115',
'00342',
'02006',
'00423',
'00461',
'00535',
'00542',
'00561',
'00566',
'00505',
'00600',
'00283',
'00114',
'02338',
'00804',
'01063',
'01076',
'01299',
'01321',
'02070',
'02043',
'01881',
'01593',
"00591",
"02464",
"02426",
"02957",
"01823",
"00811")




# 
# 
# 
#   pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI), 
#               names_from = course, values_from = rcdi_black, values_fn = length) %>% 
#   filter(SCHID %not_in% IB_schools) %>% 
#   mutate(zd_index = zscale(d_index)) %>% 
#   mutate(log_census= log10(census)) %>% 
#   mutate(zpct_frpl=zscale(pct_frpl)) %>% 
#   mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
#   group_by(id) %>% 
#   mutate(zsch_dindex = zscale(sch_dindex)) %>% 
#   mutate(log_schpop = log10(schpop)) %>% 
#   mutate(zchem = zscale(chem)) %>% 
#   ungroup() %>% 
#   lmer(chem~1+ APSAI + zsch_dindex +  log_schpop +log_census + zpct_frpl+
#        zExpenditure_per_pupil+zd_index + (1|id), data = .) %>% 
#   sjPlot::tab_model()
#   
#   

# APSAI take two ----------------------------------------------------------
  

 data_apsai <-  data_medium %>% 
    left_join(ap_count, by = "SCHID") %>% 
    mutate(APSAI = (schpop/apcount)) 
  data_apsai <- data_apsai %>% 
  filter(SCHID %not_in% IB_schools ) 
  
apsaim1<-  data_apsai %>% 
    filter(course %in% c("chem", "apany")) %>% 
    select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI) %>%
    filter(id != 924) %>% 
    pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI), 
                names_from = course, values_from = rcdi_black) %>% 
    mutate(zd_index = zscale(d_index)) %>% 
    mutate(log_census= log10(census)) %>% 
    mutate(zpct_frpl=zscale(pct_frpl)) %>% 
    mutate(zap_any = zscale(apany)) %>% 
    mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
    group_by(id) %>% 
    mutate(zsch_dindex = zscale(sch_dindex)) %>% 
    mutate(log_schpop = log10(schpop)) %>% 
    mutate(zchem = zscale(chem)) %>% 
    ungroup() %>% 
    lmer(chem~1+ APSAI + zsch_dindex +  log_schpop +log_census + zpct_frpl+
         zExpenditure_per_pupil+zd_index + (1|id), data = .)
apsaim2<-  data_apsai %>% 
  filter(course %in% c("chem", "apany")) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI) %>%
  filter(id != 924) %>% 
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI), 
              names_from = course, values_from = rcdi_black) %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zap_any = zscale(apany)) %>% 
  mutate(zAPSAI = zscale(APSAI)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zchem = zscale(chem)) %>% 
  ungroup() %>% 
  lmer(ap_any~ 1+ APSAI + zsch_dindex +  log_schpop + log_census + zpct_frpl+
         zExpenditure_per_pupil + zd_index + (1|id), data = .)
# data_apsai %>% 
#   filter(course %in% c("chem", "apany")) %>% 
#   select(School, SCHID, course, pctblack, schpctblack, rcdi_black, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI) %>%
#   filter(id != 924) %>% 
#   pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI), 
#               names_from = course, values_from = rcdi_black) %>% 
#   mutate(zd_index = zscale(d_index)) %>% 
#   mutate(log_census= log10(census)) %>% 
#   mutate(zpct_frpl=zscale(pct_frpl)) %>% 
#   mutate(zap_any = zscale(apany)) %>% 
#   mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
#   group_by(id) %>% 
#   mutate(zsch_dindex = zscale(sch_dindex)) %>% 
#   mutate(log_schpop = log10(schpop)) %>% 
#   mutate(zchem = zscale(chem)) %>% 
#   ungroup() %>% 
#   lmer(zap_any~ 1+ APSAI + zsch_dindex +  log_schpop + log_census + zpct_frpl+
#          zExpenditure_per_pupil + zd_index + (1|id), data = .) 




  sjPlot::tab_model(apsaim1, apsaim2)
summary(apsaim2)

# latinx infinities ---------------------
data_medium %>%
  filter(course %in% c("chem", "alg10910")) %>% 
  filter(id != 924) %>%
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index) %>% 
  view()
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index),
              names_from = course, values_from = rcdi_latinx) %>% 
  view()
  mutate(zd_index = zscale(d_index)) %>%
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>%
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>%
  mutate(zsch_dindex = zscale(sch_dindex)) %>%
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zalg10910 = zscale(alg10910)) %>%
  mutate(zchem = zscale(chem)) %>%
  ungroup() %>% 
  view()


# final versions of models?  ----------------------------------------------


# for CHEM outcome only. 

m_access_chem<-data_apsai %>% 
  filter(course %in% c("chem", "apany")) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI) %>%
  filter(id != 924) %>% 
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI), 
              names_from = course, values_from = rcdi_latinx) %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zap_any = zscale(apany)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zchem = zscale(chem)) %>% 
  ungroup() %>% 
  lmer(chem~1+ APSAI + zsch_dindex +  log_schpop +log_census + zpct_frpl+
         zExpenditure_per_pupil+zd_index + (1|id), data = .)
#
m_trackedness_chem <- data_medium %>% 
  filter(course =="chem") %>% 
  filter(id != 924) %>% 
  select(School, SCHID, trackedness, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio) %>%
  pivot_wider(id_cols = c(School, SCHID, id, trackedness, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio), 
              names_from = course, values_from = rcdi_latinx)  %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  mutate(zprereq = zscale(prereq_ratio)) %>% 
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  ungroup() %>% 
  select(chem,  trackedness, zsch_dindex, log_schpop, log_census, zpct_frpl, zExpenditure_per_pupil, zd_index, id, zprereq) %>% 
  lmer(chem~1+ trackedness  + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .) 
#
m_prereqs_chem <- data_medium %>% 
  filter(course %in% c("chem", "alg10910")) %>% 
  filter(id != 924) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio), 
              names_from = course, values_from = rcdi_latinx) %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  mutate(zprereq = zscale(prereq_ratio)) %>% 
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zalg10910 = zscale(alg10910)) %>% 
  mutate(zchem = zscale(chem)) %>% 
  ungroup() %>% 
  select(chem, zalg10910, zsch_dindex, log_schpop, log_census, zpct_frpl, zExpenditure_per_pupil, zd_index, id, zprereq) %>% 
  lmer(chem~1+ zprereq + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .)


#
m_timing_alg1_chem<-data_medium %>%
  filter(course %in% c("chem", "alg10910")) %>%
  filter(id != 924) %>%
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index),
              names_from = course, values_from = rcdi_latinx) %>%
  mutate(zd_index = zscale(d_index)) %>%
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>%
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>%
  mutate(zsch_dindex = zscale(sch_dindex)) %>%
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zalg10910 = zscale(alg10910)) %>%
  mutate(zchem = zscale(chem)) %>%
  ungroup() %>%
  lmer(chem~1+zalg10910 + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .)
#

# for AP Class Outcome ONLY
m_access_ap <-  data_apsai %>% 
  filter(course %in% c("chem", "apany")) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI) %>%
  filter(id != 924) %>% 
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, APSAI), 
              names_from = course, values_from = rcdi_latinx) %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zap_any = zscale(apany)) %>% 
  mutate(zAPSAI = zscale(APSAI)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zchem = zscale(chem)) %>% 
  ungroup() %>% 
  lmer(apany~ 1+ APSAI + zsch_dindex +  log_schpop + log_census + zpct_frpl+
         zExpenditure_per_pupil + zd_index + (1|id), data = .)
#
m_trackedness_ap <- data_medium %>% 
  filter(course =="apany") %>% 
  filter(id != 924) %>% 
  select(School, SCHID, course, trackedness, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio) %>%
  pivot_wider(id_cols = c(School, SCHID, id, trackedness, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio), 
              names_from = course, values_from = rcdi_latinx)  %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  mutate(zprereq = zscale(prereq_ratio)) %>% 
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  ungroup() %>% 
  select(apany,  zsch_dindex, trackedness, log_schpop, log_census, zpct_frpl, zExpenditure_per_pupil, zd_index, id, zprereq) %>% 
  lmer(apany~1+ trackedness  + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .) 
#
m_prereqs_ap <- data_medium %>% 
  filter(course =="apany") %>% 
  filter(id != 924) %>% 
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index, prereq_ratio), 
              names_from = course, values_from = rcdi_latinx)  %>% 
  mutate(zd_index = zscale(d_index)) %>% 
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>% 
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  mutate(zprereq = zscale(prereq_ratio)) %>% 
  group_by(id) %>% 
  mutate(zsch_dindex = zscale(sch_dindex)) %>% 
  mutate(log_schpop = log10(schpop)) %>% 
  ungroup() %>% 
  select(apany,  zsch_dindex, log_schpop, log_census, zpct_frpl, zExpenditure_per_pupil, zd_index, id, zprereq) %>% 
  lmer(apany~1+ zprereq + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .) 
#
m_timing_alg1_ap <- data_medium %>%
  filter(course %in% c("apany", "alg10910")) %>%
  filter(id != 924) %>%
  select(School, SCHID, course, pctblack, schpctblack, rcdi_latinx, id, sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index) %>%
  pivot_wider(id_cols = c(School, SCHID, id, schpctblack,sch_dindex, census, pct_frpl, schpop, Expenditure_per_pupil, d_index),
              names_from = course, values_from = rcdi_latinx) %>%
  mutate(zd_index = zscale(d_index)) %>%
  mutate(log_census= log10(census)) %>% 
  mutate(zpct_frpl=zscale(pct_frpl)) %>%
  mutate(zExpenditure_per_pupil=zscale(Expenditure_per_pupil)) %>%
  group_by(id) %>%
  mutate(zsch_dindex = zscale(sch_dindex)) %>%
  mutate(log_schpop = log10(schpop)) %>% 
  mutate(zalg10910 = zscale(alg10910)) %>%
  ungroup() %>%
  lmer(apany~1+zalg10910 + zsch_dindex +  log_schpop +log_census + zpct_frpl+ zExpenditure_per_pupil+zd_index + (1|id), data = .)


# final tables:

sjPlot::tab_model(m_access_chem,
                  m_trackedness_chem,
                  m_prereqs_chem,
                  m_timing_alg1_chem ,
                dv.labels = c("Access to AP",
                              "District Trackedness",
                              "Prereq Ratio",
                              "Timing Alg1")
                )

sjPlot::tab_model(m_access_ap,
                  m_trackedness_ap,
                  m_prereqs_ap,
                  m_timing_alg1_ap,
                  dv.labels = c("Access to AP",
                                "District Trackedness",
                                "Prereq Ratio",
                                "Timing Alg1"))


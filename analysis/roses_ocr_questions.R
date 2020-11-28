# ocr data clean
library(dplyr)
library(gt)
library(readr)
library(ggplot2)
library(mccrr)
library(tidyr)
library(tidyselect)
library(ggmosaic)
library(patchwork)

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
center <- function(df, var, center = 0) {
  # centers a running variable on user input center
  # first successful tidyeval function !!!
  user_var <- enquo(var)
  varname <- quo_name(user_var)
  center <- enquo(center)
  mutate(df, !!varname := (!!user_var-!!center))
}

# this is a function I wrote to fit in to `select_if`. You'll see it later. 
is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))

setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr_va\\")


chemistry <- use("Chemistry.csv")
biology <- use("Biology.csv")
algebra1 <- use("Algebra I.csv")
algebra2 <- use("Algebra II.csv")

creditrecovery <- use("Credit Recovery.csv")
gate <- use("Gifted and Talented.csv")
ap<-use("Advanced Placement.csv")
dual<-use("Dual Enrollment.csv")
ib <- use("International Baccalaureate.csv")


enrollment <- use("Enrollment.csv")


advanced_courses <- rbind(ap, dual, ib)


ap %>% filter(JJ == "No")


ocr_clean <- function(data) {  
  data %>% 
  filter(JJ == "No") %>% 
  tidyr::pivot_longer(cols = -c(LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ), 
                      names_to = "variable", values_to = "count") %>%
  dplyr::select(-c(LEAID, SCHID, COMBOKEY)) %>% 
  mutate(sex = case_when(
    grepl("_M", variable, fixed=TRUE) ~ "male",
    grepl("_F", variable, fixed=TRUE)~ "female"
  ))  %>% 
  mutate(characteristic = case_when(
    grepl("_HI_", variable, fixed = TRUE) ~ "Hispanic",
    grepl("_AM_", variable, fixed = TRUE) ~ "Am Indian / Alaska Nat",
    grepl("_AS_", variable, fixed = TRUE) ~ "Asian",
    grepl("_HP_", variable, fixed = TRUE) ~ "Nat Hawaiian / PI",
    grepl("_BL_", variable, fixed = TRUE) ~ "Black",
    grepl("_WH_", variable, fixed = TRUE) ~ "White",
    grepl("_TR_", variable, fixed = TRUE) ~ "Two or More Races",
    grepl("_LEP_", variable, fixed = TRUE) ~ "Limited English Prof",
    grepl("_IDEA_", variable, fixed = TRUE) ~ "IDEA",
    grepl("_504_", variable, fixed = TRUE) ~ "504",
  )) %>% 
  dplyr::select(-variable) %>% 
  rename(abbreviation = LEA_STATE, 
         state = LEA_STATE_NAME,
         LEA = LEA_NAME,
         School = SCH_NAME) %>% 
  filter(!is.na(count)) %>% 
  filter(!is.na(sex)) 
}
algebra1

algebra2  %>% 
  ocr_clean() %>% 
  filter(is.na(characteristic)) %>% 
  group_by(sex) %>% 
  tally(count) %>% 
  gt()

algebra2 %>% 
    tidyr::pivot_longer(cols = -c(LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ), 
                        names_to = "variable", values_to = "count") %>%
    dplyr::select(-c(LEAID, SCHID, COMBOKEY)) %>% 
  mutate(sex = case_when(
    grepl("_M", variable, fixed=TRUE) ~ "male",
    grepl("_F", variable, fixed=TRUE)~ "female"
  )) 


gate %>% 
  tidyr::pivot_longer(cols = -c(LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ), 
                      names_to = "variable", values_to = "count") %>%
  dplyr::select(-c(LEAID, SCHID, COMBOKEY)) %>% 
  mutate(sex = case_when(
    grepl("_M", variable, fixed=TRUE) ~ "male",
    grepl("_F", variable, fixed=TRUE)~ "female"
  )) 


gate %>% 
  group_by(SCH_GT_IND) %>% 
  count(
  )


gate %>% 
  filter(SCH_GT_IND == "Yes") %>% 
  select(-SCH_GT_IND) %>% 
  ocr_clean() %>%
  filter(!is.na(characteristic)) %>% 
  group_by(characteristic,) %>% 
  tally(count) %>% 
  filter(characteristic != "IDEA"  ) %>% 
  filter(characteristic != 
           "Limited English Prof") %>% 
  gt()
  
enrollment <- read_csv("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr2017\\2017-18 Public-Use Files\\Data\\SCH\\CRDC\\CSV\\Enrollment.csv")
enrollment <- enrollment %>% 
  filter(LEA_STATE == "VA")  %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         SCH_ENR_HI_M,
         SCH_ENR_HI_F,
         SCH_ENR_AM_M,
         SCH_ENR_AM_F,
         SCH_ENR_AS_M,
         SCH_ENR_AS_F,
         SCH_ENR_HP_M,
         SCH_ENR_HP_F,
         SCH_ENR_BL_M,
         SCH_ENR_BL_F,
         SCH_ENR_WH_M,
         SCH_ENR_WH_F,
         SCH_ENR_TR_M,
         SCH_ENR_TR_F
  )
enrollment[enrollment < 0] <- NA

gate %>% 
  ocr_clean() %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(characteristic) %>% 
  tally(count) %>% 
  filter(characteristic != "IDEA" ) %>% 
  filter(characteristic != 
           "Limited English Prof") %>% 
  summarize(percent = round(n/sum(n),3), 
            n = n, 
            race = characteristic) %>% 

  gt()



ap %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         SCH_APEXAM_ONEORMORE_HI_M,
         SCH_APEXAM_ONEORMORE_HI_F,
         SCH_APEXAM_ONEORMORE_AM_M,
         SCH_APEXAM_ONEORMORE_AM_F,
         SCH_APEXAM_ONEORMORE_AS_M,
         SCH_APEXAM_ONEORMORE_AS_F,
         SCH_APEXAM_ONEORMORE_HP_M,
         SCH_APEXAM_ONEORMORE_HP_F,
         SCH_APEXAM_ONEORMORE_BL_M,
         SCH_APEXAM_ONEORMORE_BL_F,
         SCH_APEXAM_ONEORMORE_WH_M,
         SCH_APEXAM_ONEORMORE_WH_F,
         SCH_APEXAM_ONEORMORE_TR_M,
         SCH_APEXAM_ONEORMORE_TR_F
         
                ) %>% 
  ocr_clean() %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(characteristic) %>% 
  tally(count) %>% 
  filter(characteristic != "IDEA" ) %>% 
  filter(characteristic != 
           "Limited English Prof") %>% 
  summarize(percent = round(n/sum(n),3), 
            n = n, 
            race = characteristic) %>% 
  
  gt()
           
dual %>% 
  filter(SCH_DUAL_IND == "Yes") %>%
  select(-SCH_DUAL_IND) %>% 
  ocr_clean() %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(characteristic) %>% 
  tally(count) %>% 
  filter(characteristic != "IDEA" ) %>% 
  filter(characteristic != 
           "Limited English Prof") %>% 
  summarize(percent = round(n/sum(n),3), 
            n = n, 
            race = characteristic) %>% 
  
  gt()


df %>% 
  select(id, starts_with("n_")) %>% 
  tidyr::pivot_longer(cols = -c(id), 
                      names_to = "variable", values_to = "count") %>%
  mutate(diploma = case_when(
    grepl("_ADV", variable, fixed = TRUE) ~ "Advanced Studies",
    grepl("_STD", variable, fixed = TRUE) ~ "Standard",
    grepl("_ STD", variable, fixed = TRUE) ~ "Standard",  
    grepl(" DO", variable, fixed = TRUE) ~ "Drop Out",      
  )) %>% 
  mutate(race = case_when(
    grepl("NA", variable, fixed = TRUE) ~ "Native American",
    grepl("Asian", variable, fixed = TRUE) ~ "Asian",
    grepl("Black", variable, fixed = TRUE) ~ "Black",
    grepl("Latin", variable, fixed = TRUE) ~ "Latinx" ,
    grepl("White", variable, fixed = TRUE) ~ "White"
  )) %>% 
  filter(!is.na(race)) %>% 
  group_by(diploma, race) %>% 
  tally(count) %>%
  summarize(percent = round(n/sum(n),3), 
            n = n, 
            race = race) %>% 
  gt()



df %>% 
  select(id, starts_with("n_")) %>% 
  tidyr::pivot_longer(cols = -c(id), 
                      names_to = "variable", values_to = "count") %>%
  mutate(diploma = case_when(
    grepl("_ADV", variable, fixed = TRUE) ~ "Advanced Studies",
    grepl("_STD", variable, fixed = TRUE) ~ "Standard",
    grepl("_ STD", variable, fixed = TRUE) ~ "Standard",  
  )) %>% 
  group_by(variable) %>% 
  count() %>% view()


algebra1 %>% 
  select(-c(SCH_MATHCLASSES_ALG,
            SCH_MATHCERT_ALG)) %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         contains("ALGENR")) %>% 
  dplyr::select(-c(SCH_ALGENR_G07_IND, SCH_ALGENR_G08_IND)) %>% 
  tidyr::pivot_longer(cols = -c(LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ), 
                      names_to = "variable", values_to = "count") %>%
  mutate(grades = case_when(
    grepl("_G08_", variable, fixed = TRUE) ~ "Grade 8, UG Middle School Age",
    grepl("_GS0910_", variable, fixed = TRUE) ~ "Grades 9-10",
    grepl("_GS1112_", variable, fixed = TRUE) ~ "Grades 11-12",  
  ))  %>%
  dplyr::select(-c(LEAID, SCHID, COMBOKEY)) %>% 
  mutate(sex = case_when(
    grepl("_M", variable, fixed=TRUE) ~ "male",
    grepl("_F", variable, fixed=TRUE)~ "female"
  ))  %>% 
  mutate(characteristic = case_when(
    grepl("_HI_", variable, fixed = TRUE) ~ "Hispanic",
    grepl("_AM_", variable, fixed = TRUE) ~ "Am Indian / Alaska Nat",
    grepl("_AS_", variable, fixed = TRUE) ~ "Asian",
    grepl("_HP_", variable, fixed = TRUE) ~ "Nat Hawaiian / PI",
    grepl("_BL_", variable, fixed = TRUE) ~ "Black",
    grepl("_WH_", variable, fixed = TRUE) ~ "White",
    grepl("_TR_", variable, fixed = TRUE) ~ "Two or More Races",
    grepl("_LEP_", variable, fixed = TRUE) ~ "Limited English Prof",
    grepl("_IDEA_", variable, fixed = TRUE) ~ "IDEA",
    grepl("_504_", variable, fixed = TRUE) ~ "504",
  )) %>% 
  dplyr::select(-variable) %>% 
  rename(abbreviation = LEA_STATE, 
         state = LEA_STATE_NAME,
         LEA = LEA_NAME,
         School = SCH_NAME) %>% 
  filter(!is.na(count)) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(grades, characteristic) %>% 
  tally(count) %>% 
  filter(characteristic != "IDEA" ) %>% 
  filter(characteristic != 
           "Limited English Prof") %>% 
  summarize(percent = round(n/sum(n),3), 
            n = n, 
            race = characteristic) %>% 
  
  gt()


algebra1 %>% 
  filter(grepl("High", SCH_NAME, fixed = TRUE)) %>% 
  group_by(LEA_NAME ) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(avg = mean(n))




chem_d_index <-   chemistry  %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School, schoolsum),
              names_from = characteristic, 
              values_from = pct) %>% 
  #dplyr::select(-Asian) %>% 
  mutate(pct_white = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pct_black = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(d_index_chem = 
           (1 - `Am Indian / Alaska Nat`^2
              - Asian^2
              - Black^2 
              - Hispanic^2
              -`Nat Hawaiian / PI`^2
              -`Two or More Races`^2
              - White^2
            )) %>% 
  ungroup() %>% 
  dplyr::select(School, d_index_chem, pct_white, pct_black, schoolsum)
chem_d_index


chemistry %>% 
  ocr_clean() %>% 
  filter(School == "Tangier Combined")


enr_d_index <- enrollment %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         contains("SCH_ENR_")) %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School, schoolsum),
              names_from = characteristic, 
              values_from = pct) %>% 
  #dplyr::select(-Asian) %>% 
  mutate(pct_white = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pct_black = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(d_index_enr = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, d_index_enr, pct_white, pct_black, schoolsum)

chem_d_index

d_index_chem %>% 
  lm(d_index_chem~d_index_enr, data = .) %>% 
  sjPlot::tab_model()

d_index_chem <- left_join(chem_d_index, enr_d_index, by = "School")
d_index_chem

pblack<-d_index_chem %>% 
  ggplot(aes(x =  pct_black.y, y = pct_black.x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_light() + 
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Percent Black (school)", 
       y = "percent Black (Chemistry)")

pwhite<-d_index_chem %>% 
  ggplot(aes(x =  pct_white.y, y = pct_white.x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_light() + 
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Percent White (school)", 
       y = "percent White (Chemistry)")
pblack+pwhite

d_alg2 <- algebra2 %>% 
  select(-c(SCH_MATHCLASSES_ALG2,
            SCH_MATHCERT_ALG2)) %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School),
              names_from = characteristic, 
              values_from = pct) %>% 
  mutate(d_index_alg2 = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, d_index_alg2)
  
d_index_alg2 <- left_join(d_alg2, enr_d_index)
d_index_alg2 %>% 
  ggplot(aes(x = d_index_enr, y = d_index_alg2 )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_light()


chemmodel <- d_index_chem %>% 
  lm(d_index_chem~d_index_enr, data = .) 
alg2model <- d_index_alg2 %>% 
  lm(d_index_alg2~d_index_enr, data = .) 

sjPlot::tab_model(chemmodel, alg2model)


d_index_alg2 %>% 
  filter(d_index_alg2 == 0) %>% 
  head(20)


#############################################
# by district

LEA_chem_d_index <-  chemistry  %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(LEA) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA),
              names_from = characteristic, 
              values_from = pct) %>% 
  mutate(d_index_chem = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(LEA, d_index_chem)
LEA_chem_d_index

LEA_enr_d_index <- enrollment %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         contains("SCH_ENR_")) %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA , characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(LEA) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA),
              names_from = characteristic, 
              values_from = pct) %>% 
  mutate(d_index_enr = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  mutate(pct_white = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pct_black = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  ungroup() %>% 
  dplyr::select(LEA, d_index_enr)
LEA_chem <- left_join(LEA_chem_d_index, LEA_enr_d_index)
LEA_chem %>% 
  ggplot(aes(x = d_index_enr, y = d_index_chem  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_light()

LEA_chem %>% 
  lm(d_index_chem ~ d_index_enr, data = .) %>% 
  sjPlot::tab_model()



d_index_chem_no_asians <- left_join(chem_d_index_no_asians, enr_d_index_no_asians)
d_index_chem_no_asians  %>% 
  ggplot(aes(x = d_index_enr, y = d_index_chem  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_light()

d_index_chem_no_asians %>% 
  lm(d_index_chem ~ d_index_enr, data = .) %>% 
  sjPlot::tab_model()



################################
# biology

d_index_bio <- biology %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School, schoolsum),
              names_from = characteristic, 
              values_from = pct) %>% 
  mutate(pct_white = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pct_black = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(d_index_bio = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, d_index_bio, pct_white, schoolsum)


d_index_bio <- left_join(bio_d_index, enr_d_index)
d_index_bio %>% 
  ggplot(aes(x = d_index_enr, y = d_index_bio   )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_light()

d_index_bio
d_index_bio %>% 
  lm(d_index_bio~d_index_enr, data = .) %>% 
  sjPlot::tab_model()
################################
# at more black schools, do fewer kids just take chem? 


chem_totals <-   chemistry  %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School),
              names_from = characteristic, 
              values_from = pct) %>% 
  #dplyr::select(-Asian) %>% 
  mutate(pct_white = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pct_black = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(d_index_chem = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            - White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, d_index_chem, pct_white, pct_black)

d_index_chem %>% 
  lm(schoolsum.x ~ pct_black.y + schoolsum.y, data= .) %>%
  sjPlot::tab_model()


d_index_chem <- left_join(chem_d_index, enr_d_index, by = "School")
library(estimatr)
d_index_chem %>% 
  mutate(pct_chem = schoolsum.x/schoolsum.y) %>% 
  filter(School != "CodeRVA Regional High") %>% 
  ggplot(aes(x = pct_black.y, y = pct_chem)) + 
  geom_point()+ geom_smooth(method = "lm_robust") +
  theme_light() + labs(x= "School pct Black", y = "School Pct Chem")

d_index_chem %>% 
  mutate(pct_chem = (schoolsum.x/schoolsum.y)*100) %>% 
  estimatr::lm_robust(pct_chem ~ pct_black.y + schoolsum.y, data = .) %>% 
  sjPlot::tab_model()
  

d_index_chem %>% 
  mutate(pct_chem = (schoolsum.x/schoolsum.y)*100) %>% 
  arrange(-pct_chem)

biochem<-d_index_chem %>% 
  mutate(
    pct_white_chem = pct_white.x, 
    pct_black_chem = pct_black.x, 
    chem_total     = schoolsum.x,
    pct_white_school = pct_white.y,
    pct_black_school = pct_black.y,
    school_enr = schoolsum.y
  ) %>% 
  dplyr::select(School, d_index_chem, pct_white_chem,pct_black_chem, chem_total, pct_white_school,
                pct_black_school, school_enr ) %>% 
  left_join(d_index_bio, by = "School") %>% 
  left_join(d_alg1, by = "School")



d_index_bio <- d_index_bio %>% 
  mutate(schoolsum_bio = schoolsum, 
         pct_white_bio = pct_white)


d_index_chem %>% 
  filter(pct_white.x == 1) %>% 
  view()
  arrange(pct_black.x) 

  
  ######################
  
  biochem
  biochem %>% 
    lm(schoolsum.x ~ pct_black.y + schoolsum.y, data= .) %>%
    sjPlot::tab_model()
  biochem %>% 
    mutate(pct_bio = (schoolsum_bio /school_enr)*100) %>% 
    estimatr::lm_robust(pct_bio ~ pct_black_school  + school_enr, data = .) %>% 
    sjPlot::tab_model()
  
  
  biochem %>% 
    mutate(pct_bio = (schoolsum_bio /school_enr)*100) %>% 
    ggplot(aes(x = pct_black_school, y = pct_bio)) + 
    geom_point()+ geom_smooth(method = "lm_robust") +
    theme_light() + labs(x= "School pct Black", y = "School Pct Chem")
  
  
  ############################
  
d_alg1 <-  algebra1 %>% 
    select(-c(SCH_MATHCLASSES_ALG,
              SCH_MATHCERT_ALG)) %>% 
    select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
           contains("ALGENR")) %>% 
    dplyr::select(-c(SCH_ALGENR_G07_IND, SCH_ALGENR_G08_IND)) %>% 
    tidyr::pivot_longer(cols = -c(LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ), 
                        names_to = "variable", values_to = "count") %>%
    mutate(grades = case_when(
      grepl("_G08_", variable, fixed = TRUE) ~ "Grade 8, UG Middle School Age",
      grepl("_GS0910_", variable, fixed = TRUE) ~ "Grades 9-10",
      grepl("_GS1112_", variable, fixed = TRUE) ~ "Grades 11-12",  
    ))  %>%
    dplyr::select(-c(LEAID, SCHID, COMBOKEY)) %>% 
    mutate(sex = case_when(
      grepl("_M", variable, fixed=TRUE) ~ "male",
      grepl("_F", variable, fixed=TRUE)~ "female"
    ))  %>% 
    mutate(characteristic = case_when(
      grepl("_HI_", variable, fixed = TRUE) ~ "Hispanic",
      grepl("_AM_", variable, fixed = TRUE) ~ "Am Indian / Alaska Nat",
      grepl("_AS_", variable, fixed = TRUE) ~ "Asian",
      grepl("_HP_", variable, fixed = TRUE) ~ "Nat Hawaiian / PI",
      grepl("_BL_", variable, fixed = TRUE) ~ "Black",
      grepl("_WH_", variable, fixed = TRUE) ~ "White",
      grepl("_TR_", variable, fixed = TRUE) ~ "Two or More Races",
      grepl("_LEP_", variable, fixed = TRUE) ~ "Limited English Prof",
      grepl("_IDEA_", variable, fixed = TRUE) ~ "IDEA",
      grepl("_504_", variable, fixed = TRUE) ~ "504",
    )) %>% 
    dplyr::select(-variable) %>% 
    rename(abbreviation = LEA_STATE, 
           state = LEA_STATE_NAME,
           LEA = LEA_NAME,
           School = SCH_NAME) %>% 
    filter(!is.na(count)) %>% 
    filter(!is.na(sex)) %>% 
    filter(!is.na(characteristic)) %>% 
    filter(grades == "Grades 9-10") %>% 
    group_by(LEA ,School, characteristic) %>% 
    tally(count) %>% 
    ungroup() %>% 
    group_by(School) %>% 
    mutate(schoolsum = sum(n)) %>% 
    mutate(pct = n/schoolsum) %>% 
    pivot_wider(id_cols = c(LEA, School, schoolsum),
                names_from = characteristic, 
                values_from = pct) %>% 
    mutate(pct_white = White / sum(
      `Am Indian / Alaska Nat`,
      Asian,
      Black, 
      Hispanic,
      `Nat Hawaiian / PI`,
      `Two or More Races`, 
      White
    )) %>% 
    mutate(pct_black = Black / sum(
      `Am Indian / Alaska Nat`,
      Asian,
      White, 
      Hispanic,
      `Nat Hawaiian / PI`,
      `Two or More Races`,
      Black
    )) %>% 
    mutate(d_index_alg1 = 
             (1 - `Am Indian / Alaska Nat`^2
              - Asian^2
              - Black^2 
              - Hispanic^2
              -`Nat Hawaiian / PI`^2
              -`Two or More Races`^2
              - White^2
             )) %>% 
    ungroup() %>% 
    dplyr::select(School, d_index_alg1, pct_white, pct_black, schoolsum)

  
d_alg1 <-d_alg1 %>% 
  mutate(schoolsum_alg1 = schoolsum,
         pct_white_alg1 = pct_white, 
         pct_black_alg1 = pct_black)
  biochem
  biochem %>% 
    mutate(pct_alg1_9_10 = (schoolsum_alg1 /school_enr)*100) %>%
    filter(pct_alg1_9_10 < 100) %>% 
    estimatr::lm_robust(pct_alg1_9_10 ~ pct_black_school  + school_enr, data = .) %>% 
    sjPlot::tab_model()
  
  
  biochem %>% 
    mutate(pct_alg1_9_10 = (schoolsum_alg1 /school_enr)*100) %>% 
    filter(pct_alg1_9_10 < 100) %>% 
    ggplot(aes(x = pct_black_school, y = pct_alg1_9_10)) + 
    geom_point()+ geom_smooth(method = "lm_robust") +
    theme_light() + labs(x= "School pct Black", y = "School Pct alg1 in 9 or 10")
  
  
  biochem %>% 
    mutate(pct_alg1_9_10 = (schoolsum_alg1 /school_enr)*100) %>% 

    arrange(-pct_alg1_9_10) %>% 
    select(School, pct_alg1_9_10)
  
  

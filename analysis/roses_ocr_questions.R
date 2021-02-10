# ocr data clean


# Libraries ---------------------------------------------------------------

library(dplyr)
library(gt)
library(readr)
library(ggplot2)
library(mccrr)
library(tidyr)
library(tidyselect)
library(ggmosaic)
library(patchwork)
library(report)
library(ggridges)
library(forcats)
library(lme4)
library(estimatr)

# functions ---------------------------------------------------------------


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
# this is a function I wrote to fit in to `select_if`. You'll see it later. 
is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))


# Pull in Data ------------------------------------------------------------

setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr_va\\")

data_long <- read_csv("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr_va\\ocr_with_analytic.csv")

full_data<-read_csv("ocr_merge_1130.csv")
data_medium <- use("data_medium.csv")
data_medium <- data_medium %>% 
  distinct(School, LEA, course, .keep_all = T)


# individual OCR sets -----------------------------------------------------


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

# Generating Tables -------------------------------------------------------
enrollment

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



# Calculating D Indices and Making New Datasets ---------------------------



# Chem D Index ------------------------------------------------------------



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
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctlatinx = Hispanic / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_chem = 
           (1 - `Am Indian / Alaska Nat`^2
              - Asian^2
              - Black^2 
              - Hispanic^2
              -`Nat Hawaiian / PI`^2
              -`Two or More Races`^2
              - White^2
            )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA,  dindex_chem, pctwhite, pctblack, pctlatinx, schoolsum)%>% 
  rename(pctwhite_chem = pctwhite, 
         schoolsum_chem = schoolsum, 
         pctblack_chem = pctblack, 
         pctlatinx_chem = pctlatinx)

# Total Enrollment D index ------------------------------------------------


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
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctlatinx = Hispanic / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_enr = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA, dindex_enr, pctwhite, pctblack, pctlatinx, schoolsum)
enr_d_index <- enr_d_index %>% 
  rename(
    pctwhite_enr = pctwhite,
    pctblack_enr = pctblack,
    pctlatinx_enr = pctlatinx,
    schoolsum_enr = schoolsum
  )




# Algebra 2 D INdex -------------------------------------------------------

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
  pivot_wider(id_cols = c(LEA, School, schoolsum),
              names_from = characteristic, 
              values_from = pct) %>% 
  mutate(pctlatinx = Hispanic / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_alg2 = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA, dindex_alg2, pctwhite, schoolsum, pctlatinx, pctblack) %>% 
  rename(pctwhite_alg2 = pctwhite, 
         pctlatinx_alg2 = pctlatinx,
         schoolsum_alg2 = schoolsum, 
         pctblack_alg2 = pctblack)
  

# Biology D INdex ---------------------------------------------------------


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
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctlatinx = Hispanic / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_bio = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA, dindex_bio, pctwhite, pctlatinx, schoolsum, pctblack) %>% 
  rename(pctwhite_bio = pctwhite, 
         pctlatinx_bio = pctlatinx,
         schoolsum_bio = schoolsum, 
         pctblack_bio = pctblack)



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

# The Biochem Thing -------------------------------------------------------

d_index_chem
biochem<-d_index_chem %>% 
  mutate(
    pct_white_school = pct_white,
    pct_black_school = pct_black,
    school_enr = schoolsum
  ) %>% 
  dplyr::select(School, d_index_chem, pct_white_chem,pct_black_chem, schoolsum_chem, pct_white_school,
                pct_black_school, school_enr ) %>% 
  left_join(d_index_bio, by = c("School", "LEA")) %>% 
  left_join(d_alg1, c("School", "LEA"))

biochem

d_index_bio <- d_index_bio %>% 
  mutate(schoolsum_bio = schoolsum, 
         pct_white_bio = pct_white)


d_index_chem %>% 
  filter(pct_white.x == 1) %>% 
  view()
  arrange(pct_black.x) 


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
  
  

# Algebra 1 D Index -------------------------------------------------------
  # d_alg1_8
  # d_alg1_9_10
  # d_alg1_11_12

  d_alg1_11_12 <-  algebra1 %>% 
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
    filter(grades == "Grades 11-12") %>% 
    group_by(LEA ,School, characteristic) %>% 
    tally(count) %>% 
    ungroup() %>% 
    group_by(School) %>% 
    mutate(schoolsum = sum(n)) %>% 
    mutate(pct = n/schoolsum) %>% 
    pivot_wider(id_cols = c(LEA, School, schoolsum),
                names_from = characteristic, 
                values_from = pct) %>% 
    mutate(pctwhite = White / sum(
      `Am Indian / Alaska Nat`,
      Asian,
      Black, 
      Hispanic,
      `Nat Hawaiian / PI`,
      `Two or More Races`, 
      White
    )) %>% 
    mutate(pctblack = Black / sum(
      `Am Indian / Alaska Nat`,
      Asian,
      White, 
      Hispanic,
      `Nat Hawaiian / PI`,
      `Two or More Races`,
      Black
    )) %>% 
    mutate(dindex_alg1 = 
             (1 - `Am Indian / Alaska Nat`^2
              - Asian^2
              - Black^2 
              - Hispanic^2
              -`Nat Hawaiian / PI`^2
              -`Two or More Races`^2
              - White^2
             )) %>% 
    ungroup() %>% 
    dplyr::select(School, LEA, dindex_alg1, pctwhite, pctblack, schoolsum) 

  
# d_alg1_8
# d_alg1_9_10
# d_alg1_11_12
  
  
  # use this one by one for all 3 algebra 1 datasets to rename them correctly the first time
  d_alg1_11_12 <-d_alg1_11_12 %>% 
  rename(schoolsum_alg11112 = schoolsum,
         pctwhite_alg11112 = pctwhite, 
         pctblack_alg11112 = pctblack, 
         dindex_alg11112    = dindex_alg1 )

  

# Gate D INdex ------------------------------------------------------------

 
d_gate <- gate %>% 
  select(-c(SCH_GT_IND)) %>% 
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
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_gate = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA, dindex_gate, pctwhite, schoolsum, pctblack) %>% 
  rename(pctwhite_gate = pctwhite, 
         schoolsum_gate = schoolsum, 
         pctblack_gate = pctblack)



# AP D Indices ------------------------------------------------------------
  # AP ONe or More Exams
d_ap_onemore <- ap %>% 
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
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
    mutate(pctlatinx = Hispanic / sum(
      `Am Indian / Alaska Nat`,
      Asian,
      Black, 
      Hispanic,
      `Nat Hawaiian / PI`,
      `Two or More Races`, 
      White
    )) %>%   
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_aponemore = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA, dindex_aponemore, pctwhite, pctlatinx,  schoolsum, pctblack) %>% 
  rename(pctwhite_aponemore = pctwhite, 
         latinx_aponemore = pctlatinx, 
         schoolsum_aponemore = schoolsum, 
         pctblack_aponemore = pctblack)

# ap ANY AP Class
d_ap_any <- ap %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         SCH_APENR_HI_M,
         SCH_APENR_HI_F,
         SCH_APENR_AM_M,
         SCH_APENR_AM_F,
         SCH_APENR_AS_M,
         SCH_APENR_AS_F,
         SCH_APENR_HP_M,
         SCH_APENR_HP_F,
         SCH_APENR_BL_M,
         SCH_APENR_BL_F,
         SCH_APENR_WH_M,
         SCH_APENR_WH_F,
         SCH_APENR_TR_M,
         SCH_APENR_TR_F
  ) %>% 
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
  mutate(pctwhite = White / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctlatinx = Hispanic / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    Black, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`, 
    White
  )) %>% 
  mutate(pctblack = Black / sum(
    `Am Indian / Alaska Nat`,
    Asian,
    White, 
    Hispanic,
    `Nat Hawaiian / PI`,
    `Two or More Races`,
    Black
  )) %>% 
  mutate(dindex_apany = 
           (1 - `Am Indian / Alaska Nat`^2
            - Asian^2
            - Black^2 
            - Hispanic^2
            -`Nat Hawaiian / PI`^2
            -`Two or More Races`^2
            -White^2
           )) %>% 
  ungroup() %>% 
  dplyr::select(School, LEA, dindex_apany, pctwhite, pctlatinx, schoolsum, pctblack) %>% 
  rename(pctwhite_apany = pctwhite, 
         pctlatinx_apany = pctlatinx, 
         schoolsum_apany = schoolsum, 
         pctblack_apany = pctblack)




# Merging allt he D index mini data sets ----------------------------------



# rose says add 1AP test, APtest at all. 


d_alg1_11_12 <- d_alg1_11_12 %>% 
  select(School, LEA, contains("alg1"), -c(dindex_alg1 ))
d_index_bio <- d_index_bio %>% 
  select(School, LEA, contains("bio"))

  

full_data <- d_index_bio %>% 
  left_join(chem_d_index, by = c("School", "LEA")) %>% 
  left_join(enr_d_index, by = c("School", "LEA")) %>% 
#  left_join(d_alg1_8, by = c("School", "LEA")) %>% 
 # left_join(d_alg1_9_10, by = c("School", "LEA")) %>%   # N = 411
 # left_join(d_alg1_11_12,by = c("School", "LEA")) %>% #N = 408
  left_join(d_alg2, by = c("School", "LEA")) %>%  #N = 364 
  left_join(d_gate, by = c("School", "LEA")) %>%  #N = 1769
  #left_join(d_ap_onemore, by = c("School", "LEA")) %>%  #N = 302
  left_join(d_ap_any, by = c("School", "LEA"))  #N = 302



data_long <- full_data %>% 
  pivot_longer(
    cols = dindex_bio:pctblack_apany,
    names_to = c(".value", "class"),
    #names_to = c("measure", "group"),
    #names_pattern = "",
    names_sep = "_",
    values_drop_na = TRUE
  ) 
data_long

# check against duplicates
full_data %>%
  group_by(School) %>% 
  count() %>% 
  arrange(-n)





# Analysis ----------------------------------------------------------------


# analysis. 
full_data %>% 
  # mutate(blackzero = if_else(pct_black_chem == 0, 1, 0)) %>% 
  # mutate(blackzero = factor(blackzero)) %>% 
  # filter(blackzero ==0) %>% 
  ggplot(aes(x = pctblack , 
             y = pct_black_chem)) + 
  geom_point()+ 
  scale_x_log10()+ 
  scale_y_log10()+
  geom_smooth(method = "lm_robust") + 
  theme_light() + 
  labs(title = "Are blacker Schools Educating Students in Chemistry?", 
       x = "Percent black School (log10)", 
       y = "Percent Black Chemistry (log10)") +
  geom_abline(slope = 1, intercept = 0, color = "red")


biochem %>% 
  distinct(School, .keep_all =T) %>% 
  estimatr::lm_robust(pct_black_chem ~ pct_black_school + school_enr, data = .) %>% 
  sjPlot::tab_model()

write_csv(full_data, "ocr_merge_1130.csv")
write_csv(data_long, "ocr_merge_1130_long.csv")
full_data 

# Rose's Next Questions ---------------------------------------------------
data_long <- use("ocr_merge_1130_long.csv")


# Andrew's Super Vizzzz ---------------------------------------------------


class_order2<- c( "alg18"    , 
 "gate"    ,
 "aponemore",
 "apany"    ,
 "chem"    ,
 "alg11112"  ,
 "alg2"      ,
 "enr"       ,
 "bio"     ,  
 "alg1910" )  

data_long %>% 
  mutate(clr = if_else(class =="enr", 1, 0))  %>% 
    ggplot(aes(x = pctblack, y = fct_relevel(.f = class, levels = class_order2), fill = factor(clr))) + 
  geom_density_ridges() +
  scale_y_discrete(
    labels = c(
    "Algebra 1 in 8th", 
    "Gifted"    ,
    "AP Test",
    "AP Class"    ,
    "Chemistry"    ,
    "Algebra 1 in 11-12"  ,
    "Algebra 2"      ,
    "School Enrollment"       ,
    "Biology"     ,  
    "Algebra 1 in 9-10"  )
  )+
  scale_x_continuous(limits = c(0,1))+
  theme_ridges() + 
  university::scale_fill_uva() +
  #geom_boxplot(alpha = 0.7)+
  theme_light() +
  guides(fill = FALSE) + 
  labs(title = "Black students are less likely to be 
in more rigorous High School courses", 
       y = "",
       x = "Percent Black")

data_long  %>% 
  group_by(class) %>% 
  count()
# T-Test series -----------------------------------------------------------

# THe HLM pathway is shut. The dead keep it.
mchem <- data_long %>% 
  filter(class %in% c("chem", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .)
mgifted <-  data_long %>% 
  filter(class %in% c("gate", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
mbio <-  data_long %>% 
  filter(class %in% c("bio", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
maponemore <-  data_long %>% 
  filter(class %in% c("aponemore", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
mapany <-  data_long %>% 
  filter(class %in% c("apany", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
malg2 <-  data_long %>% 
  filter(class %in% c("alg2", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
malg1910<-  data_long %>% 
  filter(class %in% c("alg1910", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
malg18 <-  data_long %>% 
  filter(class %in% c("alg18", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .) 
malg11112 <-  data_long %>% 
  filter(class %in% c("alg11112", "enr")) %>% 
  lme4::lmer(pctblack ~ 1 + class + (1 | School ), data = .)


dvs<-c(
"Algebra 1 in 11-12"  ,
"Algebra 1 in 8th",
"Algebra 1 in 9-10",
"Algebra 2"      ,
"AP Class"    ,
"AP Test",
"Biology"     ,
"Chemistry"    ,
"Gifted"    
  )
sjPlot::tab_model( c( malg11112,   malg18,   malg1910,  malg2,   mapany, 
                    maponemore,  mbio,mchem,   mgifted),   dv.labels = dvs
)

# Let's try good-old within-sample t-tests. ... 
full_data

# "Algebra 1 in 11-12"  
  t.test(full_data$pctblack_alg11112 , full_data$pctblack_enr,  paired = TRUE) %>% report()
  # Significant
  
# "Algebra 1 in 8th"
  t.test(full_data$pctblack_alg18 , full_data$pctblack_enr,  paired = TRUE) %>% report()
  #not
  
# "Algebra 1 in 9-10"
  t.test(full_data$pctblack_alg1910 , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # significant
  
# "Algebra 2"      
  t.test(full_data$pctblack_alg2 , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # significant
  
# "AP Class"    
  t.test(full_data$pctblack_apany , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # significant
  
# "AP Test"
  t.test(full_data$pctblack_aponemore , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # significant
  
# "Biology"     
  t.test(full_data$pctblack_bio , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # not
  
# "Chemistry"   
  t.test(full_data$pctblack_chem , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # significant

# "Gifted"
  t.test(full_data$pctblack_gate , full_data$pctblack_enr,  paired = TRUE)%>% report()
  # significant
  
gatemean<- data_long %>% 
  filter(class == "gate") %>% 
  select(pctblack) %>% 
  summarize(mu = mean(pctblack, na.rm = T))
enrmean <- data_long %>% 
  filter(class == "enr") %>% 
  select(pctblack) %>% 
  summarize(mu = mean(pctblack, na.rm = T))


  data_long %>% 
    filter(class %in% c("gate", "enr")) %>% 
    ggplot(aes( x = pctblack, fill = class)) +
    geom_density(alpha = 0.5) + 
    geom_vline(xintercept = 0.135, color ="#232D4B") + 
    geom_vline(xintercept = 0.256, color = "#E57200") + 
    university::scale_fill_uva() +
    theme_light() + 
    annotate("text", x = 0.5, y = 3.2, label = "Vertical lines represent mean values")

  
  


 leas <- full_data %>% 
    select(LEA) %>% 
    group_by(LEA) %>% 
    count()
write_csv(leas, "leas.csv")    
leas<-read_csv("leas.csv")
leas

data_long <- data_long %>% 
  left_join(leas, by ="LEA") %>% 
  select(School, id, everything())
analytic_data <-read_csv("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\analytic_data.csv")
analytic_data


data_long <- data_long %>% 
  left_join(analytic_data, by = "id")


data_long
write_csv(data_long, "ocr_with_analytic.csv")


data_long <- data_long %>% 
 rename(trackedness = metric) %>% 
  mutate(course = if_else(class == "enr", "aall_enrolled", class)) %>% 
  dplyr::select(School, id, course, trackedness,  dindex, pctblack, pctlatinx, everything())

malg11112 <- data_long %>% 
  filter(course == "alg11112" | course == "aall_enrolled")  
  lmer(pctblack ~1 +  course + trackedness+  d_index +log10(census) + (1 | id) , data = .) 



malg18 <- data_long %>% 
  filter(course == "alg18" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course +trackedness+   d_index + log10(census)+(1 | id) , data = .) 



malg1910 <- data_long %>% 
  filter(course == "alg1910" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course + trackedness+  d_index+ log10(census) +(1 | id) , data = .)



malg2 <- data_long %>% 
  filter(course == "alg2" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course + trackedness+  d_index +  log10(census)+(1 | id) , data = .) 



mapany <- data_long %>% 
  filter(course == "apany" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course +trackedness+  d_index + log10(census)+(1 | id) , data = .) 



maponemore <- data_long %>% 
  filter(course == "aponemore" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course + trackedness+  d_index +log10(census)+(1 | id) , data = .)



mbio <- data_long %>% 
  filter(course == "bio" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course +trackedness+ d_index +  log10(census)+(1 | id) , data = .)



mchem <- data_long %>% 
  filter(course == "chem" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course +trackedness+  d_index +log10(census)+(1 | id) , data = .) 

mgifted <- data_long %>% 
  filter(course == "gate" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course + trackedness+ d_index + log10(census) +(1 | id) , data = .)



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


analytic_data %>% 
  correlate(pct_black, pct_frpl)
sjPlot::tab_model(mchem)

mchem_int <-  data_long %>% 
  filter(class == "chem" | class == "enr")  %>% 
  lmer(pctblack ~1 +  class*metric + d_index  +log10(census)+(1 | id) , data = .) 
sjPlot::tab_model()


names(data_long)
data_long %>% 
  filter(class == "chem" | class == "enr")  %>% 
  lmer(dindex ~1 +  class + d_index +metric +log10(census)+(1 | id) , data = .)  %>% 
  sjPlot::tab_model()


?sjPlot::tab_model()


#
# First, as that report emphasizes, Latinx students show up in all the bad trends. Can we run the same model that we ran for Black 
# students with Latinx students? It doesn't really matter what we find with them-- we just need to have a line or two saying that we 
# looked at them as well. 
# The paper has Latinx students woven in throughout so is weird not to have looked at the relationship between
# tracking and their representation. 

# Second, am attaching a copy of the paper. I flagged a few sections for you to look at. I did a rough 
# pass of the opening/lit review, adding in some algebra 1 content, and of the discussion so that the new findings are I think reflected 
# throughout. 

# I also did fast pass on the methods and results as well. It doesn't need to be perfect to go to Maggie, but I want all of 
# the elements in there so she just has to do the framing. 

# Length wise, we are fine. It is supposed to be 30 to 40 pages and we are 35 so 
# still have a bit of space. Layering the new findings onto the short form of the paper was really helpful for keeping us on target length wise!


# just latinx -------------------------------------------------------------



malg2 <- data_long %>% 
  filter(course == "alg2" | course == "aall_enrolled")  %>% 
  lmer(pctlatinx ~1 +  course + trackedness+  d_index +  log10(census)+(1 | id) , data = .) 



mapany <- data_long %>% 
  filter(course == "apany" | course == "aall_enrolled")  %>% 
  lmer(pctlatinx ~1 +  course +trackedness+  d_index + log10(census)+(1 | id) , data = .) 


mbio <- data_long %>% 
  filter(course == "bio" | course == "aall_enrolled")  %>% 
  lmer(pctlatinx ~1 +  course +trackedness+ d_index +  log10(census)+(1 | id) , data = .)


mchem <- data_long %>% 
  filter(course == "chem" | course == "aall_enrolled")  %>% 
  lmer(pctlatinx ~1 +  course +trackedness+  d_index +log10(census)+(1 | id) , data = .) 


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
data_long %>% 
  filter(course == "chem" | course == "aall_enrolled") 
  


# andrew's brain pain -----------------------------------------------------

data_long


data_long %>% 
  filter(course == "chem" | course == "aall_enrolled")   


mchem <- data_long %>% 
  filter(course == "chem" | course == "aall_enrolled")  %>% 
  lmer(pctblack ~1 +  course +  trackedness + d_index + log10(census)+(1 | id) , data = .) 

stupidmodel1<-data_long %>% 
  filter(course == "chem" | course == "aall_enrolled") %>% 
  select(id, course, pctblack,  trackedness, d_index, census) %>% 
  mutate(course = if_else(course == "chem", 1, 0))  %>% 
  lmer(pctblack ~1 +  course +  trackedness + d_index + log10(census)+(1 | id) , data = .)
stupidmodel2<-data_long %>% 
  filter(course == "chem" | course == "aall_enrolled") %>% 
  select(id, course, pctblack,  trackedness, d_index, census) %>% 
  mutate(course = if_else(course == "aall_enrolled", 1, 0))  %>% 
  lmer(pctblack ~1 +  course +  trackedness + d_index + log10(census)+(1 | id) , data = .)  

stupidmodel3<-data_long %>% 
  filter(course == "chem") %>% 
  select(id, course, pctblack,  trackedness, d_index, census) %>% 
  lmer(pctblack ~1 +    trackedness + d_index + log10(census)+(1 | id) , data = .)
stupidmodel4<-data_long %>% 
  filter(course == "chem" | course == "aall_enrolled") %>%
  select(id, course, pctblack,  trackedness, d_index, census) %>% 
  lmer(pctblack ~1 +  trackedness + d_index + log10(census)+(1 | id) , data = .)  
labellabel<-c("predict:chem", "predict:school", "chemrestricted", "unclassed")
sjPlot::tab_model(stupidmodel1, stupidmodel2, stupidmodel3, stupidmodel4, 
                  dv.labels = labellabel)

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



# fixing the brain pain
data_long

# okay so we need the y variable to become "discrepancy" 
# for each of the 4 class categories and for the 2 comparisons (black and latinx)
# actually that's just it. Okay. 

# first let's get rid of the extra variables becuase this will be a doozy if we keep 'em
data_medium <- data_long %>% 
  select(School, id, course, trackedness, dindex, pctblack, pctlatinx, LEA, pctwhite, schoolsum, 
         division, d_index, urban, mostly, rural, Expenditure_per_pupil, census, pct_frpl)


data_medium %>% 
  select(School, course, trackedness, pctblack, pctlatinx, pctwhite, 
         schpctblack, schpctlatinx, schpctwhite, discrepancy_black, 
         discrepancy_latinx)
# somewhat better

school_pcts <- data_medium %>% 
  filter(course == "aall_enrolled") %>% 
  dplyr::select(School, pctblack, pctlatinx, pctwhite, schoolsum) %>% 
  rename(
    schpctblack = pctblack, 
    schpctlatinx = pctlatinx, 
    schpctwhite = pctwhite,
    schpop = schoolsum
  )


school_pcts
data_medium <- data_medium %>% 
  filter(course != "aall_enrolled")

data_medium %>% 
  left_join(school_pcts, by = "School") %>% 
  dplyr::select(School, course, trackedness, dindex, pctblack, pctlatinx, pctwhite, 
                schpctblack, schpctlatinx,schpctwhite, everything()) %>% 
  replace_na(list(pctlatinx  = 0, pctblack  =0)) %>% 
  mutate(discrepancy_black = (pctblack - schpctblack),
         discrepancy_latinx = (pctlatinx -schpctlatinx)) ->data_medium

write_csv(data_medium, "data_medium.csv")


data_medium %>% 
  ggplot(aes(x = discrepancy_black)) + 
  geom_histogram() +
  facet_wrap(~course)


# re-do models with discrepancy y -----------------------------------------

names(data_medium)
data_medium %>% 
  filter(course == "apany") %>% 
  select(School,  LEA, pctblack, pctwhite, dindex, schoolsum)

data_medium %>% 
  group_by(course) %>% 
  count()


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

sjPlot::tab_model(mapany, show.se=TRUE)

#
# plot_model(mchem) + plot_model(mbio)
# redo with pure numbers
malg2 <- data_medium %>% 
  filter(course == "alg2") %>% 
  lmer(pctblack  ~1 +  schpctwhite + trackedness+d_index +  log10(census)+(1 | id) , data = .) 

#  other stuff ------------------------------------------------------------




mapany <- data_medium %>% 
  filter(course == "apany" )  %>% 
  lmer(pctblack  ~1 +  schpctwhite +trackedness+ d_index+ log10(census)+(1 | id) , data = .) 


mbio <- data_medium %>% 
  filter(course == "bio" )  %>% 
  lmer(pctblack  ~1 + schpctwhite+ trackedness+ d_index +  log10(census)+(1 | id) , data = .)


mchem <- data_medium %>% 
  filter(course == "chem")  %>% 
  lmer(pctblack  ~1 +  schpctwhite+ trackedness+ d_index +log10(census)+(1 | id) , data = .) 
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
library(apaTables)
apa.reg.table(mbio, filename = "chem_model.doc")


data_medium

bf_map<-data_medium %>% 
  filter(course == "apany" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  mutate(slots = 100*(schoolsum/schpop)) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness + slots + schpctwhite 
        + Expenditure_per_pupil+ log10(schpop) + log10(census)+
            urban + rural + pct_frpl + 
          (1 | id) , data = .) 


bf_map

bf_alg2<-data_medium %>% 
  filter(course == "alg2" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  rename(slots = schoolsum) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness + slots + schpctwhite 
        + Expenditure_per_pupil+  log10(schpop) + log10(census)+
          urban + rural + pct_frpl + 
          (1 | id) , data = .)  
bf_chem<-data_medium %>% 
  filter(course == "bio" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  rename(slots = schoolsum) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness + slots + schpctwhite 
        + Expenditure_per_pupil+  log10(schpop) + log10(census)+
          urban + rural + pct_frpl + 
          (1 | id) , data = .)  
bf_bio<-data_medium %>% 
  filter(course == "chem" )  %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  rename(slots = schoolsum) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lmer( discrepancy_black ~1 + trackedness + slots + schpctwhite 
        + Expenditure_per_pupil+  log10(schpop) + log10(census)+
          urban + rural + pct_frpl + 
          (1 | id) , data = .)  


sjPlot::tab_model( c( 
  # malg11112,  
  # malg18,   
  # malg1910,  
  bf_alg2,   bf_map, 
  #maponemore,  
  bf_bio,
  bf_chem#,  
  #mgifted
),   dv.labels = dvs, 
show.se = TRUE
)



220*2.7
data_medium %>% 
  filter(course == "apany") %>% 
  mutate(schpctwhite = (schpctwhite*100)) %>% 
  mutate(slotspercents = (schoolsum/schpop)) %>% 
  mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
  lm(slotspercents ~  log10(schpop) + log10(census)  , data = .) %>% 
 # lmer(slotspercents ~  log10(schpop) + log10(census) +  (1 | id) , data = .)%>% 
  # sjPlot::plot_model()
  sjPlot::tab_model()


data_medium %>% 
  filter(course == "chem") -> justchem
  t.test(justchem$pctblack, justchem$schpctblack,  paired = TRUE) %>% report()

  
  ###
  data_medium %>% 
    filter(course == "apany") %>% 
    mutate(schpctwhite = (schpctwhite*100)) %>% 
    mutate(slotspercents = (schoolsum/schpop)) %>% 
    mutate(discrepancy_black  = (discrepancy_black*100)) %>% 
    arrange(-discrepancy_black) %>% 
    select(School, course, discrepancy_black, everything()) %>% 
    select(School, discrepancy_black, LEA) 
    group_by(School, LEA) %>% 
    count() %>% 
    arrange(-n)
  
    
data_medium <- use("data_medium.csv")
data_medium  %>% 
  select(School, LEA, id, course)%>% 
  distinct(School, LEA, .keep_all = T)


data_medium %>% 
  filter(School == "Heritage High") %>% 
  select(School, LEA, course, discrepancy_black) %>% 
  arrange(course, School)


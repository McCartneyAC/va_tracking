# ocr data clean


# Packages  ---------------------------------------------------------------

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
library(apaTables)
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
    dplyr::select(-c(LEAID,  COMBOKEY)) %>% 
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

is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))



# Read in Individual Sets -------------------------------------------------
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


# Begin Cleaning: Assign SchID --------------------------------------------

enrollment %>% 
  select(LEA_NAME, SCHID, SCH_NAME) #are you kidding. it was there all along? 

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



# Clean Individual Subjects -----------------------------------------------


## Total School Enrollment -------------------------------------------------



enroll_clean <- enrollment %>% 
  select(SCH_NAME, LEA_STATE,LEA_STATE_NAME,LEAID,LEA_NAME,SCHID,SCH_NAME,COMBOKEY, JJ,
         contains("SCH_ENR_")) %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,SCHID, School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID, School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_enr, pctwhite, pctblack, pctlatinx, schoolsum) %>% 
  rename(
    pctwhite_enr = pctwhite,
    pctblack_enr = pctblack,
    pctlatinx_enr = pctlatinx,
    schoolsum_enr = schoolsum
  )

## Chemistry ---------------------------------------------------------------

 chem_clean <-  chemistry  %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, SCHID, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(SCHID) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School, schoolsum, SCHID),
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
  dplyr::select(School, SCHID, LEA,  dindex_chem, pctwhite, pctblack, pctlatinx, schoolsum)%>% 
  rename(pctwhite_chem = pctwhite, 
         schoolsum_chem = schoolsum, 
         pctblack_chem = pctblack, 
         pctlatinx_chem = pctlatinx)

## Biology -----------------------------------------------------------------


bio_clean <- biology %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,School, SCHID, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, School, schoolsum, SCHID),
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
  dplyr::select(School,SCHID,  LEA, dindex_bio, pctwhite, pctlatinx, schoolsum, pctblack) %>% 
  rename(pctwhite_bio = pctwhite, 
         pctlatinx_bio = pctlatinx,
         schoolsum_bio = schoolsum, 
         pctblack_bio = pctblack) 


## Algebra 2 ---------------------------------------------------------------


alg2_clean <- algebra2 %>% 
  select(-c(SCH_MATHCLASSES_ALG2,
            SCH_MATHCERT_ALG2)) %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA ,SCHID, School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID, School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_alg2, pctwhite, schoolsum, pctlatinx, pctblack) %>% 
  rename(pctwhite_alg2 = pctwhite, 
         pctlatinx_alg2 = pctlatinx,
         schoolsum_alg2 = schoolsum, 
         pctblack_alg2 = pctblack)


## AP Class Clean ----------------------------------------------------------

apclass_clean <- ap %>% 
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
  group_by(LEA ,SCHID, School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID, School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_apany, pctwhite, pctlatinx, schoolsum, pctblack) %>% 
  rename(pctwhite_apany = pctwhite, 
         pctlatinx_apany = pctlatinx, 
         schoolsum_apany = schoolsum, 
         pctblack_apany = pctblack)
 

## GATE --------------------------------------------------------------------



gate_clean<-gate %>% 
  select(-c(SCH_GT_IND)) %>% 
  ocr_clean() %>% 
  filter(characteristic != "IDEA") %>% 
  filter(characteristic != "Limited English Prof") %>% 
  filter(!is.na(characteristic)) %>% 
  group_by(LEA, SCHID,School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID, School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_gate, pctwhite, schoolsum, pctblack) %>% 
  rename(pctwhite_gate = pctwhite, 
         schoolsum_gate = schoolsum, 
         pctblack_gate = pctblack)


# Alg1 8th ----------------------------------------------------------------
alg1_08_clean <-   algebra1 %>% 
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
  dplyr::select(-c(LEAID, COMBOKEY)) %>% 
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
  filter(grades == "Grade 8, UG Middle School Age") %>% 
  group_by(LEA , SCHID, School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID,  School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_alg1, pctwhite, pctblack, schoolsum)  %>% 
  rename(schoolsum_alg108 = schoolsum,
         pctwhite_alg108 = pctwhite, 
         pctblack_alg108 = pctblack, 
         dindex_alg108  = dindex_alg1 )


# Alg1 9-10 ---------------------------------------------------------------
alg1_09_10_clean <-   algebra1 %>% 
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
  dplyr::select(-c(LEAID, COMBOKEY)) %>% 
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
  group_by(LEA , SCHID, School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID,  School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_alg1, pctwhite, pctblack, schoolsum)  %>% 
  rename(schoolsum_alg10910 = schoolsum,
         pctwhite_alg10910 = pctwhite, 
         pctblack_alg10910 = pctblack, 
         dindex_alg10910    = dindex_alg1 )




# Alg1 11-12 --------------------------------------------------------------

alg1_11_12_clean <-   algebra1 %>% 
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
  dplyr::select(-c(LEAID, COMBOKEY)) %>% 
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
  group_by(LEA , SCHID, School, characteristic) %>% 
  tally(count) %>% 
  ungroup() %>% 
  group_by(School) %>% 
  mutate(schoolsum = sum(n)) %>% 
  mutate(pct = n/schoolsum) %>% 
  pivot_wider(id_cols = c(LEA, SCHID,  School, schoolsum),
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
  dplyr::select(School, SCHID, LEA, dindex_alg1, pctwhite, pctblack, schoolsum) %>% 
  rename(schoolsum_alg11112 = schoolsum,
         pctwhite_alg11112 = pctwhite, 
         pctblack_alg11112 = pctblack, 
         dindex_alg11112    = dindex_alg1 )



# ReJoining all Minor Sets ------------------------------------------------

enroll_clean       # N = 1958
chem_clean         # N = 342
bio_clean          # N = 342
alg2_clean         # N = 358  # PROBLEM CHILD (TYPE ERROR)
apclass_clean      # N = 302
gate_clean         # N = 1769
alg1_08_clean      # N = 389
alg1_09_10_clean   # N = 389
alg1_11_12_clean   # N = 408

full_data<- alg1_11_12_clean %>% 
  full_join(alg1_09_10_clean) %>% 
  left_join(alg1_08_clean) %>% 
  full_join(chem_clean) %>% 
  full_join(bio_clean) %>% 
  left_join(gate_clean) %>% 
  left_join(apclass_clean) %>% 
  left_join(alg2_clean) %>% 
  left_join(enroll_clean) 
full_data
data_long <- full_data %>% 
   pivot_longer(
     cols = dindex_alg11112:schoolsum_enr,
     names_to = c(".value", "class"),
     #names_to = c("measure", "group"),
     #names_pattern = "",
     names_sep = "_",
     values_drop_na = TRUE
   ) 

write_csv(full_data, "ocr_merge_0104.csv")
write_csv(data_long, "ocr_merge_0104_long.csv")
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr_va\\")

data_long<- read_csv("ocr_merge_0104_long.csv")

# Joining LEAs and Analytic -----------------------------------------------


# leas <- full_data %>% 
#   select(LEA) %>% 
#   group_by(LEA) %>% 
#   count()
#write_csv(leas, "leas.csv")     # SHUT THE DOOR AFTER YOU JESUS
                                 # were you born in a barn? 
leas<-read_csv("leas.csv")
leas

data_long <- data_long %>% 
  left_join(leas, by ="LEA")  

data_long <- data_long %>% 
  select(School, id, everything())
analytic_data <-read_csv("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\analytic_data.csv")
analytic_data
data_long <- data_long %>% 
  left_join(analytic_data, by = "id")


data_long
write_csv(data_long, "ocr_with_analytic.csv")



# Getting to Medium -------------------------------------------------------

data_long <- data_long %>% 
  rename(trackedness = metric) %>% 
  mutate(course = if_else(class == "enr", "aall_enrolled", class)) %>% 
  dplyr::select(School, id, course, trackedness,  dindex, pctblack, pctlatinx, everything())


data_medium <- data_long %>% 
  select(School, SCHID, id, course, trackedness, dindex, pctblack, 
         pctlatinx, LEA, pctwhite, schoolsum, division, d_index, 
         urban, mostly, rural, Expenditure_per_pupil, census, pct_frpl)

school_pcts <- data_medium %>% 
  filter(course == "aall_enrolled") %>% 
  dplyr::select(School, SCHID, pctblack, pctlatinx, pctwhite, schoolsum) %>% 
  rename(
    schpctblack = pctblack, 
    schpctlatinx = pctlatinx, 
    schpctwhite = pctwhite,
    schpop = schoolsum
  )



data_medium <- data_medium %>% 
  filter(course != "aall_enrolled")

data_medium %>% 
  left_join(school_pcts) %>% 
  dplyr::select(School, SCHID, course, trackedness, dindex, pctblack, pctlatinx, pctwhite, 
                schpctblack, schpctlatinx,schpctwhite, everything()) %>% 
  replace_na(list(pctlatinx  = 0, pctblack  =0)) %>% 
  mutate(discrepancy_black = (pctblack - schpctblack),
         discrepancy_latinx = (pctlatinx -schpctlatinx)) ->data_medium

write_csv(data_medium, "data_medium.csv")

# check that it worked: 
data_medium %>% 
  ggplot(aes(x = discrepancy_black)) + 
  geom_histogram() +
  facet_wrap(~course)
data_medium %>% 
  group_by(School,SCHID, LEA) %>% 
  count() %>% 
  arrange(-n) 

data_medium %>% 
  filter(SCHID == "00204")
## Getting two of everything in rockbrdige because the 
## id 81 is duplicated in analytic_data.csv

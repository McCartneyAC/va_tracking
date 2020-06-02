
library(readxl)
library(magrittr)
library(ggthemes)
# devtools::install_github("McCartneyAC/mccrr")
library(mccrr)
library(extrafont)
library(psych)
library(tidyverse)
library(lme4)
library(edlf8360)
library(sjPlot)

# setwd("C:\\Users\\mccar\\Desktop\\tracking")
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files"); getwd()



# functions --------------------
# 
uvapal <- c("#E57200","#232D4B", "#007681","#F2CD00","#692A7E", "#84BD00","#A5ACAF", "#5C7F92","#857363","#CAC0B6")
theme_textbook <- function () {
  theme_light() + theme(text = element_text(family = "Times New Roman"))
}

paste_data <- function(header=TRUE,...) {
  require(tibble)
  x<-read.table("clipboard",sep="\t",header=header,stringsAsFactors=TRUE,...)
  as_tibble(x)
}



# clean data ----------------------
df1<-read_xlsx("book2.xlsx")
df2<-read_xlsx("book3.xlsx")
df3<-read_xlsx("book4.xlsx")
spending<-read_xlsx("expenditure.xlsx") 
spending <-spending %>% 
  select(-`School Division`)

pos<- read_csv("pos_clean_3.csv")

pos %<>% 
  mutate(id = dist_id) %>% 
  dplyr::select(-dist_id) %>% 
  dplyr::select(id, everything()) %>% 
  dplyr::select(id, SUM, `Division num`) %>% 
  print()

df1 %<>% 
  mutate(id = `dist id`) %>% 
  dplyr::select(-`dist id`)

df2 %<>% 
  mutate(id = `Div. No.`) %>% 
  # "Div Num" is not division number, it's district ID. 
  dplyr::select(-`Div. No.`)

df3 %<>% 
  mutate(id = `division number`) %>% 
  # "`division number` is not division number, it's district ID. 
  dplyr::select(-`division number`)

df <- df1 %>% 
  left_join(df2, by = "id") %>% 
  left_join(df3, by = "id") %>% 
  left_join(spending, by = "id") %>% 
  select(id, everything()) %>% 
  arrange(id) %>% 
  mutate(census = `Census Total`)

df <- df %>% 
  left_join(pos, by = "id")

#gen dummies and other variables
df <- df %>% 
  mutate(urban = if_else(urbanicity == "urban", 1, 0)) %>% 
  mutate(mostly = if_else(urbanicity == "mostly_rural", 1, 0)) %>% 
  mutate(rural = if_else(urbanicity == "rural", 1, 0)) %>% 
  mutate(pct_frpl = (`Total FRPL` / `Total Enrollment`)) %>% 
  mutate(pct_apib = (AB_IB_Dual_Enrollment / `Total Enrollment`)) %>% 
  mutate(pct_adv = `n_Advanced`/(`n_Advanced` + `n_ Standard Diploma` + 
                                   `n_Drop out` + GED + `n_ Applied`)) %>% 
  mutate(pct_hisp  = (Hispanic_any / `Total Enrollment`)) %>% 
  mutate(pct_ai_na = (AI_NA / `Total Enrollment`)) %>% 
  mutate(pct_asian = (Asian / `Total Enrollment`)) %>% 
  mutate(pct_black = (Black_or_AA / `Total Enrollment`)) %>% 
  mutate(pct_nwopi = (NW_OPI / `Total Enrollment`)) %>% 
  mutate(pct_white = (White / `Total Enrollment`)) %>% 
  mutate(pct_2more = (`Two or more races`/ `Total Enrollment`)) %>% 
  mutate(d_index = (1 - pct_hisp^2 - pct_ai_na^2 - pct_asian^2 - 
                      pct_black^2 - pct_nwopi^2 - pct_white^2 - pct_2more^2)) %>% 
  mutate(metric = SUM/13) %>% 
  mutate(division = `Division num`)

names(df)


# Dataset Complete ----------------
df <- df %>% 
  select(id, Name ,division,  
         d_index, metric, 
         urban, mostly, rural,
         Expenditure_per_pupil, census,  
         pct_frpl, pct_apib, 
         pct_adv, pct_hisp, pct_ai_na, pct_asian, 
         pct_black, pct_nwopi, pct_white, pct_2more, 
         `Census Percent Rural`) %>% 
  rename(pct_rural = `Census Percent Rural`) 



df %>% 
  describe(fast = TRUE)

write_excel_csv(df, "analytic_data.csv")

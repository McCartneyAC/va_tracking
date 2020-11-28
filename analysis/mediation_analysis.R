library(mccrr)
library(tidyverse)
library(sjPlot)
library(gt)
library(lme4)
library(readxl)
library(magrittr)
library(ggthemes)
# devtools::install_github("McCartneyAC/mccrr")
library(mccrr)
library(extrafont)
library(psych)
library(edlf8360)
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


setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files"); getwd()

df<-use("analytic_data.csv") 
df <-df %>%  #establish raw counts
  rowwise() %>% 
  mutate(
    raw_hisp = pct_hisp*census, 
    raw_aina = pct_ai_na*census,
    raw_asian = pct_asian*census,
    raw_black= pct_black*census,
    raw_nwopi = pct_nwopi*census,
    raw_white = pct_white*census,
    raw_2more = pct_2more*census
  ) %>% 
  mutate(
    raw_nonwhite = sum(raw_hisp, raw_aina, raw_asian, raw_black, raw_nwopi, raw_2more), 
    raw_nonwhite_adv = sum(`n_ADV Asian`, `n_ADV Black`,`n_ADV Latinx`, `n_ADV NA`), 
    pct_adv_nonwhite = (raw_nonwhite_adv / raw_nonwhite) 
  )
df 
names(df)


df <- df %>% 
  select(pct_adv_nonwhite, d_index,metric, census, pct_frpl, raw_nonwhite, raw_nonwhite_adv) %>%  
  tidyr::drop_na() %>% 
  mutate(census = log10(census))
df

library(MeMoBootR)
mediation_model <- mediation1(
  y = "pct_adv_nonwhite",
  x = "d_index",
  m = "metric",
  cvs = c("census","pct_frpl"),
  df = df, 
  with_out = T, 
  nboot = 1000,
  conf_level = 0.95
)
tab_model(
  mediation_model$model1,
  mediation_model$model2,
  mediation_model$model3
)


####################################

model1<-estimatr::lm_robust(
  metric ~ d_index + pct_frpl + log10(census), 
  data = df, 
  clusters = division
)
tab_model(model1)
m_direct <- estimatr::lm_robust(
  pct_nonwhite_adv ~ d_index + pct_frpl + log10(census), 
  data = df, 
  clusters = division
)
tab_model(m_direct)

df %>% 
  ggplot(aes(x = d_index, y = pct_nonwhite_adv)) + 
  geom_jitter() + geom_smooth(method = "lm")

m_indirect <- estimatr::lm_robust(
  pct_nonwhite_adv ~ metric + pct_frpl + log10(census), 
  data = df, 
  clusters = division
)
tab_model(m_indirect)


tab_model(model1, m_direct, m_indirect)
mediation_model$model1 %>% summary()

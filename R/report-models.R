#' This script generates the data frames for the models of section 4. Chain Event Graphs
#' and experiments in defining the defining the splits between continuous variables
#' 

library(tidyverse)
library(dplyr)

# Data Processing ---------------------------------------------------------

df_full <- read.table("../data/tab/unified_database_2014.tab", sep = "\t", header = TRUE)

impute_missing <- function(x) {ifelse(x < 0, NA, x)}

df <- df_full %>% 
  select(ID, sex = Sex_all, age = Age_all, miles = t1_a2, freq = t1_a1, 
         nlacc1 = t1_nlacc_orig, hp =  t0_takeHP) %>%
  apply(2, impute_missing) %>%
  as.data.frame() %>%
  mutate(sex = fct_recode(factor(sex), F = "0", M = "1"),
         hp = factor(hp),
         acc_inv = factor(case_when(
           is.na(nlacc1)  ~ NA_character_, 
           nlacc1 >= 1 ~ "1+",
           TRUE ~ "0")),
         miles = if_else(freq == "7", 0, miles),
         miles = miles * 2,
         freq_lab = fct_recode(factor(freq),
                               "Everyday" = "1",
                               "4-6 days per week" = "2",
                               "1-3 days per week" = "3",
                               "About a fortnight" = "4",
                               "About once a month" = "5",
                               "Less than once a month" = "6",
                               "Never" = "7"))

# Frequency model ---------------------------------------------------------

unique(cut_number(df$age, 3))

mod_freq_df <- df %>%
  mutate(age_group = cut(age, c(16, 18, 21, 80))) %>%
  drop_na(sex, age, freq, acc_inv) %>%
  mutate(freq = if_else(freq %in% c("3","4"), "3", as.character(freq)),
         freq = factor(freq),
         hp = factor(hp)) %>%
  filter(freq %in% c("1","2","3")) %>%
  select(age_group, sex, freq, acc_inv, hp)

write.csv(mod_freq_df, "../data/processed/freq_mod.csv")

# Mileage model ---------------------------------------------------------

unique(cut_number(df$miles * 2, 3))

mod_miles_df <- df %>%
  filter(freq != 7) %>%
  mutate(age_group = cut(age, c(16, 18, 21, 80))) %>%
  drop_na(sex, age) %>%
  mutate(freq = if_else(freq %in% c("3","4"), "3", as.character(freq)),
         freq = factor(freq),
         hp = factor(hp),
         miles = case_when(
           miles > 12000 ~ "1",
           miles <= 12000 & miles > 4000 ~ "2",
           miles <= 4000 ~ "3",
           TRUE ~ NA_character_
         )) %>%
  filter(freq %in% c("1","2","3")) %>%
  select(age_group, sex, miles, acc_inv)

write.csv(mod_freq_df, "../data/processed/miles_mod.csv")

# Raw data for finding the age and mileage thresholds ---------------------

base_mod_df <- df %>%
  drop_na(sex, age, freq, acc_inv) %>%
  mutate(freq = if_else(freq %in% c("3","4"), "3", as.character(freq)),
         freq = factor(freq)) %>%
  filter(freq %in% c("1","2","3")) %>%
  select(age, sex, freq, miles, acc_inv)

write.csv(base_mod_df, "../data/processed/base_mod.csv")
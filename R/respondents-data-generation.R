#' This script generates the data frame for the missing responses model
library(tidyverse)
library(xtable)

ltdq <- read.table("../data/tab/ltdq.tab", sep = "\t", header = TRUE)
deq1 <- read.table("../data/tab/deq1.tab", sep = "\t", header = TRUE)
deq2 <- read.table("../data/tab/deq2.tab", sep = "\t", header = TRUE)
deq3 <- read.table("../data/tab/deq3.tab", sep = "\t", header = TRUE)
deq4 <- read.table("../data/tab/deq4.tab", sep = "\t", header = TRUE)

waves <- paste0("wave", seq(1,4))

missing_df <- ltdq %>%
  select(ID, pass, cohort, sex) %>%
  left_join(deq1[,c("ID", "V00160")]) %>%
  left_join(deq2[,c("ID", "V00357")]) %>%
  left_join(deq3[,c("ID", "V00628")]) %>%
  left_join(deq4[,c("ID", "V00853")]) %>%
  mutate(wave1 = if_else(is.na(V00160), 0, 1),
         wave2 = if_else(V00357 != "-8", 1, 0),
         wave3 = if_else(V00628 != "-8", 1, 0),
         wave4 = if_else(V00853 != "-8", 1, 0),
         sent = if_else((pass == 1 | wave1 == 1 | wave2 == 1 | wave3 == 1 | wave4 == 1), 1, 0),
         sex = fct_recode(factor(sex), F = "1", M = "2"),
         ) %>%
  select(sex, cohort, sent, wave1, wave2, wave3, wave4)

#' wave1 = all cohorts, wave2 = all cohorts
#' wave3 = cohorts 1-12, wave4 = cohorts 1-8

missing_df[is.na(missing_df)] <- 0
missing_df[missing_df$cohort > 12, "wave3"] <- NA
missing_df[missing_df$cohort > 8, "wave4"] <- NA

# Respondents by cohort
missing_cohort_df <- missing_df %>% 
  group_by(cohort) %>% 
  summarise(LTDQ = n(), DEQ = sum(sent), DEQ1 = sum(wave1), 
            DEQ2 = sum(wave2), DEQ3 = sum(wave3), DEQ4 = sum(wave4)) %>%
  mutate(cohort = LETTERS[cohort])
missing_cohort_df[is.na(missing_cohort_df)] <- 0
colSums(missing_cohort_df[,2:ncol(missing_cohort_df)])
print(xtable(missing_cohort_df, type = "latex", digits = 0), include.rownames = FALSE)

# Respondents by sex
missing_sex_df <- missing_df %>% 
  group_by(sex) %>% 
  summarise(LTDQ = n(), DEQ1 = sum(wave1), 
            DEQ2 = sum(wave2), DEQ3 = sum(wave3), DEQ4 = sum(wave4))  %>%
  column_to_rownames("sex") %>% t() %>% as.data.frame()
missing_sex_df[,"F_pct"] = round(missing_sex_df[,"F"] * 100 / rowSums(missing_sex_df), 1)
missing_sex_df[,"M_pct"] = round(missing_sex_df[,"M"] * 100 / rowSums(missing_sex_df), 1)
print(xtable(missing_sex_df[,c("F", "F_pct", "M", "M_pct")], type = "latex", digits = 1))

# Save to csv
write.csv(missing_df, file = "../data/processed/new_missing_values.csv")
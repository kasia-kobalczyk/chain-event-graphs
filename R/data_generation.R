#' This script generates the data frame for missing values
library(tidyverse)

# df_full <- read.table("data/unified_database_2014.tab", sep = "\t", header = TRUE)
# ltdq <- read.table("data/ltdq.tab", sep = "\t", header = TRUE)
# 
# impute_missing <- function(x) {ifelse(x < 0, NA, x)}
# 
# df <- df_full %>% 
#   select(ID, Cohort_all, sex = Sex_all, age = Age_all,
#          freq1 = t1_a1,
#          freq2 = t2_a1,
#          freq3 = t3_a1,
#          freq4 = t4_a1 ) %>%
#   as.data.frame() %>%
#   mutate(sex = fct_recode(factor(sex), F = "0", M = "1"),
#          wave1 = 1,
#          wave2 = if_else(freq2 == "-99", 0, 1),
#          wave3 = if_else(freq3 == "-99", 0, 1),
#          wave4 = if_else(freq4 == "-99", 0, 1)) %>%
#   select(ID, wave1, wave2, wave3, wave4) %>%
#   na.omit()
# # 
# # missing_df <- ltdq %>%
# #   select(ID, cohort, sex, pass) %>%
# #   filter(pass == 1) %>%
# #   left_join(df)
# # 
# # missing_df %>%
# #   filter(cohort == 9) %>%
# #   select(cohort, pass, wave1, wave2, wave3, wave4) %>%
# #   colSums(na.rm = TRUE)
# 
# 
# write.csv(df, file = "data/missing_values.csv")


# =======================

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

# wave1 = all cohorts
# wave2 = all cohorts
# wave3 = cohorts 1-12
# wave4 = cohorts 1-8

missing_df[is.na(missing_df)] <- 0
missing_df[missing_df$cohort > 12, "wave3"] <- NA
missing_df[missing_df$cohort > 8, "wave4"] <- NA

colSums(missing_df[,c("sent",waves)])

write.csv(missing_df, file = "../data/processed/new_missing_values.csv")

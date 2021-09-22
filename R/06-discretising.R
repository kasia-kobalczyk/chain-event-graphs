#' This script contains the code for determining the best value of splits
#' between different categories of age and mileage using modified staged trees
#' 

library(stagedtrees)
library(ggplot2)
library(tidyverse)
theme_set(theme_minimal())

data <- read_csv("../data/processed/base_mod.csv")
head(data)

data <- data %>%
  mutate(int_age = round(age)) %>%
  drop_na(sex, acc_inv)

data %>% ggplot(aes(x = int_age)) +
  geom_bar() +
  facet_wrap(~sex, ncol = 1)

data %>% ggplot(aes(x = int_age, fill = acc_inv)) +
  geom_bar(pos = "fill")

data %>% ggplot(aes(x = int_age, fill = acc_inv)) +
  geom_bar(pos = "fill") +
  facet_wrap(~sex, ncol = 1)


# ----------------------------------------------------------------------------

# Age -> Accident Involvement
order <- c("int_age", "acc_inv")
N_INIT <- length(unique(data$int_age))
full_mod <- full(data, order, join_unobserved = FALSE)
greedy <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv")
full_search_3 <- full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv", n_bins = 3)
full_search_4 <- full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv", n_bins = 4)
plot(greedy)
plot(full_search_3)
plot(full_search_4)

  
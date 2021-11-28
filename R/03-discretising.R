#' This script contains the code for determining the best value of splits
#' between different categories of age and mileage using modified staged trees

library(stagedtrees)
library(ggplot2)
library(tidyverse)
theme_set(theme_minimal())

data <- read_csv("../data/processed/base_mod.csv")
data <- data %>%
  mutate(int_age = round(age)) %>%
  drop_na(sex, acc_inv)

# Plots --------------------------------------------------------------

data %>% ggplot(aes(x = int_age)) +
  geom_bar() +
  facet_wrap(~sex, ncol = 1)

data %>% ggplot(aes(x = int_age, fill = acc_inv)) +
  geom_bar(pos = "fill")

data %>% ggplot(aes(x = int_age, fill = acc_inv)) +
  geom_bar(pos = "fill") +
  facet_wrap(~sex, ncol = 1)

# Discretising the Age -------------------------------------------------

# Age -> Accident Involvement
order <- c("int_age", "acc_inv")
N_INIT <- length(unique(data$int_age))
full_mod <- full(data, order, join_unobserved = FALSE)
greedy <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv")
plot(greedy)
full_search_4 <- stages_full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv", n_bins = 4)
plot(full_search_4)

# Sex + Age -> Accident Involvement
order <- c("sex", "int_age", "acc_inv")
N_INIT <- length(unique(data$int_age))
full_mod <- full(data, order, join_unobserved = FALSE)
greedy <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv")
plot(greedy)
full_search_3 <- stages_full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv", n_bins = 3)
plot(full_search_3)
full_search_4 <- stages_full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv", n_bins = 4)
plot(full_search_4)

## Females only
order <- c("int_age", "acc_inv")
subset <- data %>% filter(sex == "F")
N_INIT <- length(unique(subset$int_age))
full_mod <- full(subset, order, join_unobserved = FALSE)
greedy_f <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv")
full_f <- stages_full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv")

## Males only
subset <- data %>% filter(sex == "M")
N_INIT <- length(unique(subset$int_age))
full_mod <- full(subset, order, join_unobserved = FALSE)
greedy_m <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv")
full_m <- stages_full_ordered_search(full_mod, n_init = N_INIT, variable = "acc_inv")

par(mfrow = c(1, 2))
plot(greedy_f)
title("females")
plot(greedy_m)
title("males")
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(full_f)
title("females")
plot(full_m)
title("males")
par(mfrow = c(1, 1))

# Testing the methods -------------------------------------------------------

#' Runs a model selection method and returns the fitted model together with the 
#' ranges produced by the method and the running time of the algorithm
test_method <- function(data, order, var, target_var, method, n_bins = NULL) {
  n_init <- nrow(unique(data[,var]))
  full_mod <- full(data, order, join_unobserved = FALSE)
  if (method == "greedy") {
    start_time <- Sys.time()
    mod <- stages_ordered_bhc(full_mod, n_init = n_init, variable = target_var)
    end_time <- Sys.time()
    time <- end_time - start_time
  }
  else if (method == "full") {
    if (is.na(n_bins)) {
      stop("Final number of bins must be specified")
    }
    start_time <- Sys.time()
    mod <- stages_full_ordered_search(full_mod, n_init = n_init, 
                                      variable = target_var, n_bins = n_bins)
    end_time <- Sys.time()
    time <- end_time - start_time
  }
  else {
    stop("Invalid method")
  }
  values <- mod$tree[[var]]
  stages <- mod$stages[[target_var]]
  counts <- mod$ctables[[var]]
  temp_df <- data.frame(value = values, stage = stages, count = counts)
  temp_df <- temp_df %>% 
    group_by(stage) %>%
    summarise(min = min(value), max = max(value), 
              range = paste0(min, "-", max)) %>%
    arrange(min)
  ranges <- unique(temp_df$range)
  return(list(model = mod,
              ranges = ranges, 
              time = time))
}

order1 <- c("int_age", "acc_inv")
greedy1 <- test_method(data, order1, var = "int_age", target_var = "acc_inv", method = "greedy")
full1_4 <- test_method(data, order1, var = "int_age", target_var = "acc_inv", method = "full", n_bins = 4)
full1_5 <- test_method(data, order1, var = "int_age", target_var = "acc_inv", method = "full", n_bins = 5)

order2 <- c("sex", "int_age", "acc_inv")
greedy2 <- test_method(data, order2, var = "int_age", target_var = "acc_inv", method = "greedy")
full2_3 <- test_method(data, order2, var = "int_age", target_var = "acc_inv", method = "full", n_bins = 3)
full2_4 <- test_method(data, order2, var = "int_age", target_var = "acc_inv", method = "full", n_bins = 4)

order3 <- c("freq", "sex", "int_age", "acc_inv")
greedy3 <- test_method(data, order3, var = "int_age", target_var = "acc_inv", method = "greedy")
full3_2 <- test_method(data, order3, var = "int_age", target_var = "acc_inv", method = "full", n_bins = 2)
full3_3 <- test_method(data, order3, var = "int_age", target_var = "acc_inv", method = "full", n_bins = 3)


#' Fits two models - one with the var split into equally sized groups,
#' one with the var split based on the provided ranges. 
#' Generates the statistics for boths models
compare_cegs <- function(data, prev_var, var, target_var, range) {
  breaks <- c(as.numeric(substr(range, 1, 2)), Inf)
  mod_df <- data %>%
    mutate(simple_var_group = cut_number(data[[var]], length(range)),
           new_var_group = cut(data[[var]], breaks = breaks, 
                               include.lowest = TRUE))
  order <- c(prev_var, "simple_var_group", target_var)
  simple_mod <- mod_df %>%
    full(order, join_unobserved = FALSE) %>%
    stages_bhc()
  order <- c(prev_var, "new_var_group", target_var)
  new_mod <- mod_df %>%
    full(order, join_unobserved = FALSE) %>%
    stages_bhc()
  stats <- c("BIC", "AIC", "df")
  simple_stats <- c(BIC(simple_mod), AIC(simple_mod), attr(simple_mod$ll, "df"))
  names(simple_stats) <- stats
  new_stats <- c(BIC(new_mod), AIC(new_mod), attr(new_mod$ll, "df"))
  names(new_stats) <- stats
  return(list(
    simple_mod = simple_mod,
    new_mod = new_mod,
    simple_stats  = simple_stats,
    new_stats  = new_stats
  ))
}

compare1 <- compare_cegs(data, NULL , "int_age", "acc_inv", greedy1$ranges)
plot(compare1$simple_mod)
plot(compare1$new_mod)

compare2 <- compare_cegs(data, "sex", "int_age", "acc_inv", greedy2$ranges)
plot(compare2$simple_mod)
plot(compare2$new_mod)

compare3 <- compare_cegs(data, c("freq", "sex"), "int_age", "acc_inv", full3_2$ranges)
plot(compare3$simple_mod)
plot(compare3$new_mod)


# Export mod2 data to plot with GraphViz in python
range <- greedy2$ranges
breaks <- c(as.numeric(substr(range, 1, 2)), 80)
mod_df <- data %>%
  mutate(ef_age = cut_number(int_age, length(range)),
         new_age = cut(int_age, breaks = breaks, 
                             include.lowest = TRUE)) %>%
  select(sex, ef_age, new_age, acc_inv)

write.csv(mod_df, file = "../data/processed/split_age.csv")

# Discretising the mileage ---------------------------------------------------

data2 <- data %>%
  filter(miles > 0) %>%
  mutate(miles_cat = cut_width(miles, 2000),
         miles_cat = droplevels.factor(miles_cat)) %>%
  drop_na()
order <- c("miles_cat", "acc_inv")
plot(full(data2, order))
greedy_miles <- test_method(data2, order, var = "miles_cat", target_var = "acc_inv", method = "greedy")
full_miles_3 <- test_method(data2, order, var = "miles_cat", target_var = "acc_inv", method = "full", n_bins  = 3)
full_miles_4 <- test_method(data2, order, var = "miles_cat", target_var = "acc_inv", method = "full", n_bins  = 4)
full_miles_5 <- test_method(data2, order, var = "miles_cat", target_var = "acc_inv", method = "full", n_bins  = 5)
plot(greedy$model)
plot(full_miles_3$model)
plot(full_miles_4$model)
plot(full_miles_5$model)


# mod_df <- df %>%
#   filter(freq != 7) %>%
#   mutate(freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4"))) %>%
#   group_by(sex) %>%
#   mutate(age_group = cut_number(age, N_INIT)) %>%
#   select(freq, hp, sex, age, age_group, acc_inv) %>%
#   na.omit()

# --------------------------- Equally sized initial groups
N_INIT <- 20
mod_df <- df %>%
  filter(freq != 7) %>%
  mutate(age_group = cut_number(age, N_INIT),
         freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4"))) %>%
  select(freq, hp, sex, age, age_group, acc_inv) %>%
  na.omit()

order <- c("sex", "age_group", "acc_inv")

frequency <- c(1,2)

# Greedy all
full_mod <- full(mod_df %>% filter(freq %in% frequency) %>% select("age_group", "acc_inv"))
mod_greedy_all <- stages_ordered_bhc(full_mod, n_init = 20, variable = "acc_inv", per_subset = TRUE)
plot(mod_greedy_all)
title("Males and females - greedy")

# Exhaustive all
full_mod <- full(mod_df %>% filter(freq %in% frequency) %>% select("age_group", "acc_inv"))
mod_exhaustive_all <- exhaustive_ordered_search(full_mod, n_bins = 4, variable = "acc_inv")
plot(mod_exhaustive_all)
title("Males and females - full search")

# Greedy - males and females split: per subset = FALSE
full_mod <- full(mod_df %>% filter(freq %in% frequency) %>% select("sex", "age_group", "acc_inv"))
mod_greedy_all <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv", per_subset = FALSE)
plot(mod_greedy_all)
title("Males and females - greedy search")

# Greedy - males and females split: per subset = TRUE
full_mod <- full(mod_df %>% filter(freq %in% frequency) %>% select("sex", "age_group", "acc_inv"))
mod_greedy_all <- stages_ordered_bhc(full_mod, n_init = N_INIT, variable = "acc_inv", per_subset = TRUE)
plot(mod_greedy_all)
title("Males and females - greedy search")

# Exhaustive - females
full_mod <- full(mod_df %>% filter(freq %in% frequency & sex == "F") %>% select("age_group", "acc_inv"))
mod_exhaustive_f <- exhaustive_ordered_search(full_mod, n_bins = 3, variable = "acc_inv")

# Exhaustive - males
full_mod <- full(mod_df %>% filter(freq %in% frequency & sex == "M") %>% select("age_group", "acc_inv"))
mod_exhaustive_m <- exhaustive_ordered_search(full_mod, n_bins = 3, variable = "acc_inv")

par(mfrow = c(1,2))
plot(mod_exhaustive_m)
title("males")
plot(mod_exhaustive_f)
title("females")
par(mfrow = c(1,1))



# ------------------------------------------ Integer initial values

mod_df <- df %>%
  filter(freq != 7) %>%
  mutate(int_age = floor(age),
         freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4"))) %>%
  select(freq, sex, age, int_age, acc_inv, miles) %>%
  na.omit()

order <- c("sex", "int_age", "acc_inv")

frequency <- c(1,2)

# Greedy all
full_mod <- full(mod_df %>% filter(freq %in% frequency) %>% select("int_age", "acc_inv"))
mod_greedy_all <- stages_ordered_bhc(full_mod, n_init = 53, variable = "acc_inv")
plot(mod_greedy_all)
title("Males and females - greedy")

# Exhaustive all
full_mod <- full(mod_df %>% filter(freq %in% frequency) %>% select("int_age", "acc_inv"))
mod_exhaustive_all <- exhaustive_ordered_serach(full_mod, n_bins = 3, variable = "acc_inv")
plot(mod_exhaustive_all)
title("Males and females - full search")

# Exhaustive - females
full_mod <- full(mod_df %>% filter(freq %in% frequency & sex == "F") %>% select("int_age", "acc_inv"))
mod_exhaustive_f <- exhaustive_ordered_serach(full_mod, n_bins = 3, variable = "acc_inv")

mod_greedy_f <- stages_ordered_bhc(full_mod, n_init = 52, variable = "acc_inv")

par(mfrow = c(1,2))
plot(mod_greedy_f)
title("Females - greedy")
plot(mod_exhaustive_f)
title("Females - full search")
par(mfrow = c(1,1))

# Exhaustive - males
full_mod <- full(mod_df %>% filter(freq %in% frequency & sex == "M") %>% select("int_age", "acc_inv"))
mod_exhaustive_m <- exhaustive_ordered_serach(full_mod, n_bins = 2, variable = "acc_inv")

mod_greedy_m <- stages_ordered_bhc(full_mod, n_init = 47, variable = "acc_inv")

par(mfrow = c(1,2))
plot(mod_greedy_m)
title("Males - greedy")
plot(mod_exhaustive_m)
title("Males - full search")
par(mfrow = c(1,1))

# -------------------------------------------------------------------------

mod_df %>%
  filter(freq %in% frequency) %>%
  select(order) %>%
  group_by(sex, int_age, acc_inv) %>%
  count() %>%
  pivot_wider(names_from = acc_inv, values_from = n) %>%
  mutate(sample_size = !!sym("0") + !!sym("1+"),
         prob_0 = !!sym("0")  / sample_size,
         prob_1 = !!sym("1+")  / sample_size)

mod_df %>% 
  filter(freq %in% frequency) %>% 
  filter(sex == "M") %>%
  select(order) %>%
  group_by(sex, int_age, acc_inv) %>%
  count() %>%
  ggplot(aes(x = int_age, y = n, fill = acc_inv)) +
  geom_bar(stat = "identity", pos = "fill")

mod_df %>%
  filter(freq %in% frequency) %>%
  filter(sex == "M") %>%
  mutate(age_group = cut(age, breaks = c(16, 19.1, 27.3, 80))) %>%
  ggplot(aes(x = age_group, fill = acc_inv)) +
  geom_bar(pos = "fill")


mod_merged <- stages_bhc(mod, scope = "acc_inv")
plot(mod_merged)

# ===================
mod_df2 <- mod_df %>%
  mutate(new_age_group = cut(age, breaks = c(16, 17.5, 18, 19.3, 34.8, 80)),
         age_group = cut_number(age, n = 5),
         new_sex_age = paste0(sex, "-", new_age_group))

full_mod <- full(mod_df2[,order])
plot(full_mod)

mod <- stages_bhc(full_mod)
plot(mod)
plot(ceg(mod))
barplot(mod, "acc_inv")



order <- c("age_group", "sex", "freq", "acc_inv")
mod_freq <- stages_bhc(full(mod_df2[,order]))
plot(mod_freq)

order <- c("new_age_group", "sex", "freq", "acc_inv")
new_mod_freq <- stages_bhc(full(mod_df2[,order]))
plot(new_mod_freq)

order <- c("new_age_group", "sex", "freq", "acc_inv")
new_mod_freq2 <- stages_hc(indep(mod_df2[,order]))
plot(new_mod_freq2)

BIC(mod_freq, new_mod_freq, new_mod_freq2)
AIC(mod_freq, new_mod_freq, new_mod_freq2)

# ===================
order <- c("age_group", "sex", "freq", "acc_inv")

full_mod <- full(mod_df[, order])
plot(full_mod)

mod <- stages_ordered_bhc(full_mod, variable = "sex", n_init = N_INIT)
plot(mod)

mod <- stages_bhc(mod)
plot(mod)

# -------------------------------------------------------------------------
order <- c("age_group", "sex", "acc_inv")

full_mod <- full(mod_df[,order])
plot(full_mod)

mod <- stages_ordered_bhc(full_mod, variable = "sex", n_init = N_INIT)
mod <- stages_bhc(mod, scope = "acc_inv")
plot(mod)

# ===================
mod_df3 <- mod_df %>%
  mutate(age_group = cut(age, breaks = c(16, 17.4, 17.5, 18, 20.1, 80)))

full_mod <- full(mod_df3[,order])
plot(full_mod)

mod <- stages_bhc(full_mod)
plot(mod)
plot(ceg(mod))
barplot(mod, "acc_inv")

# -------------------------------------------------------------------------
order <- c("age_group", "acc_inv")

full_mod <- full(mod_df[, order])
plot(full_mod)

mod <- stages_ordered_bhc(full_mod, variable = "acc_inv", n_init = N_INIT)
plot(mod)

# ===================
order <- c("freq", "sex", "age_group", "acc_inv")
mod_df4 <- mod_df %>%
  mutate(age_group = cut(age, breaks = c(16, 17.5, 21.2, 34.8, 80)))

full_mod <- full(mod_df4[,order])
mod <- stages_bhc(full_mod, ignore = "NA")
plot(mod)
plot(ceg(mod))
barplot(mod, "acc_inv")



# Hazard Perception -------------------------------------------------------

mod_df1 <- mod_df %>% mutate(
  age_group = cut(age, breaks = c(16, 18.2, 19.1, 32, 80))
)
base_order <- c("sex", "age_group", "acc_inv")
hp_order  <- c("sex", "age_group", "hp", "acc_inv")

base_mod <- stages_bhc(full(mod_df1 %>% filter(freq == 1) %>% select(base_order)),
                       score = function(x){-AIC(x)})
hp_mod <- stages_bhc_plot(full(mod_df1 %>% filter(freq == 1) %>% select(hp_order)), 
                          score = function(x){-AIC(x)},
                          plot_var = "acc_inv")


par(mfrow = c(1,2))
plot(base_mod)
plot(hp_mod)
par(mfrow = c(1,1))


#  ------------------------------------------------------------------------
mod_df <- df %>%
  filter(freq != 7) %>%
  mutate(int_age = floor(age),
         freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4"))) %>%
  select(freq, sex, age, int_age, acc_inv, miles) %>%
  na.omit()

mod_df %>%
#  filter(sex == "M") %>%
  ggplot(aes(x = int_age, fill = acc_inv)) +
  geom_bar(pos = "fill")

order <- c("int_age", "acc_inv")
full_mod <- full(mod_df %>% filter(sex == "M") %>% select(order), lambda = 1)
mod_male <- exhaustive_ordered_serach(full_mod, variable = "acc_inv")
plot(mod_male)

full_mod <- full(mod_df %>% filter(sex == "F") %>% select(order), lambda = 1)
mod_female <- exhaustive_ordered_serach(full_mod, variable = "acc_inv")
plot(mod_female)

full_mod <- full(mod_df %>% select(order),  lambda = 1)
mod_all4 <- exhaustive_ordered_serach(full_mod, n_bins = 4, variable = "acc_inv")
mod_all3 <- exhaustive_ordered_serach(full_mod, n_bins = 3, variable = "acc_inv")
base_mod_all <- stages_bhc(full_mod)
greedy_mod_all <- stages_ordered_bhc(full_mod, n_init = length(full_mod$stages$acc_inv), variable = "acc_inv")

BIC(base_mod_all, greedy_mod_all, mod_all3, mod_all4)

plot(greedy_mod_all)
plot(mod_all4)
plot(mod_all3)
plot(base_mod_all)

male_age_cuts <- c(16, 19, 30, 80)
female_age_cuts <- c(16, 20, 32, 80)
all_age_cuts <- c(16, 18, 20, 32, 80)

mod_df2 <- mod_df %>%
  mutate(age_group = cut(age, breaks = all_age_cuts, 
                         include.lowest = TRUE, right = FALSE),
         equal_age_group = cut_number(int_age, 4))

order <- c("age_group", "sex", "acc_inv")
final_mod <- mod_df2 %>% 
  select(all_of(order)) %>%
  full(lambda = 0) %>%
  stages_bhc()

par(mfrow = c(1,2))
plot(final_mod)
barplot(final_mod, var = "acc_inv")
par(mfrow = c(1,1))


order <- c("equal_age_group", "sex", "acc_inv")
equal_mod <- mod_df2 %>% 
  select(all_of(order)) %>%
  full(lambda = 0) %>%
  stages_bhc()

par(mfrow = c(1,2))
plot(equal_mod)
barplot(equal_mod, var = "acc_inv")
par(mfrow = c(1,1))

#  ------------------------------------------------------------------------
df %>%
  filter(freq != 7) %>%
  mutate(freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4")),
         miles_bin = cut(miles, breaks = seq(0, 60000, 1000))) %>%
  select(freq, sex, age, acc_inv, miles, miles_bin) %>%
  na.omit() %>%
  ggplot(aes(x = miles_bin, fill = acc_inv)) +
    geom_bar()
  
mod_df <- df %>%
  filter(freq != 7) %>%
  mutate(int_age = floor(age),
         freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4")),
         miles_bin = factor(cut(miles, breaks = c(0, 500, seq(1000,20000, 1000), 30000, Inf)))) %>%
  select(freq, sex, age, int_age, acc_inv, miles, miles_bin) %>%
  na.omit()

order <- c("miles_bin", "acc_inv")
full_mod <- full(mod_df[,order], lambda = 0)
greedy_mod <- stages_ordered_bhc(full_mod, n_init = length(full_mod$stages$acc_inv), variable = "acc_inv")
best_mod3 <- exhaustive_ordered_serach(full_mod, n_bins = 3, variable = "acc_inv")
best_mod4 <- exhaustive_ordered_serach(full_mod, n_bins = 4, variable = "acc_inv")
best_mod5 <- stages_bhc(exhaustive_ordered_serach(full_mod, n_bins = 5, variable = "acc_inv"))
default_mod <- stages_bhc(full_mod)

plot(greedy_mod)
plot(best_mod5)

miles_cut3 <- c(0, 2000, 6000, Inf)
miles_cut4 <- c(0, 500, 2000, 6000, Inf)
miles_cut5 <- c(0, 500, 2000, 6000, 17000, Inf)

mod_df2 <- mod_df %>%
  mutate(miles3 = cut(miles, breaks = miles_cut3),
         miles4 = cut(miles, breaks = miles_cut4),
         miles5 = cut(miles, breaks = miles_cut5))

mod3 <- stages_bhc(full(mod_df2[,c("miles3", "acc_inv")], lambda = 0))
mod4 <- stages_bhc(full(mod_df2[,c("miles4", "acc_inv")], lambda = 0))
mod5 <- stages_bhc(full(mod_df2[,c("miles5", "acc_inv")], lambda = 0))

plot(mod3)
plot(mod4)
plot(mod5)

BIC(mod3, mod4, mod5)
BIC(best_mod3, best_mod4, best_mod5)

#  Age and mileage  -----------------------------------------------------------

mod_df <- mod_df %>%
  mutate(age_group = cut(age, breaks = all_age_cuts, 
                         include.lowest = TRUE, right = FALSE),
         mileage = cut(miles, breaks = miles_cut4),
         even_age = cut_number(int_age, 4),
         even_mileage = cut_number(miles, 4))

order <- c("age_group", "sex", "mileage", "acc_inv")
mod_miles <- stages_bhc(full(mod_df[,order], lambda = 0))
plot(mod_miles)

order <- c("even_age", "sex", "even_mileage", "acc_inv")
even_mod_miles <- stages_bhc(full(mod_df[,order], lambda = 0))
plot(even_mod_miles)

BIC(mod_miles, even_mod_miles)

order <- c("age_group", "sex", "freq", "acc_inv")
mod_freq <- stages_bhc(full(mod_df[,order], lambda = 0))
plot(mod_freq)

order <- c("even_age", "sex", "freq", "acc_inv")
even_mod_freq <- stages_bhc(full(mod_df[,order], lambda = 0))
plot(even_mod_freq)

BIC(mod_freq, even_mod_freq)


#  Miles per day  -----------------------------------------------------------

mod_df <- df %>%
  filter(freq != 7) %>%
  mutate(miles = miles * 2,
         n_days = case_when(
           freq == "1" ~ 365,
           freq == "2" ~ 365 * 5/7,
           freq == "3" ~ 365 * 2/7,
           freq == "4" ~ 365 * 1/14,
           freq == "5" ~ 365 * 1/30,
           freq == "6" ~ 365 * 1/60),
         daily_mileage = miles / n_days,
         age_group = cut(age, breaks = all_age_cuts, 
                         include.lowest = TRUE, right = FALSE),
         freq = factor(if_else(freq %in% c(1,2,3), as.character(freq), "4"))) %>%
  select(freq, hp, sex, age, age_group, acc_inv, daily_mileage) %>%
  na.omit()

mod_df %>% 
  mutate(daily_cut = cut(daily_mileage, breaks = c(seq(0,100, 5), Inf))) %>%
  ggplot(aes(x = daily_cut, fill = acc_inv)) +
  geom_bar(pos = "fill")

mod_df <- mod_df %>% mutate(
  daily_cut = cut(daily_mileage, breaks = c(seq(0,100, 5), Inf))
)

order <- c("daily_cut", "acc_inv")
full_mod <- full(mod_df[,order], lambda = 0)
basic_mod <- stages_bhc(full_mod)
greedy_mod <- stages_ordered_bhc(full_mod, n_init = length(full_mod$stages$acc_inv), variable = "acc_inv")
best_mod3 <- exhaustive_ordered_serach(full_mod, n_bins = 3, variable = "acc_inv")
best_mod4 <- exhaustive_ordered_serach(full_mod, n_bins = 4, variable = "acc_inv")

plot(basic_mod)
plot(greedy_mod)
plot(best_mod3)
plot(best_mod4)

daily_cuts3 <- c(0, 15, 35, Inf)
daily_cuts4 <- c(0, 15, 35, 95, Inf)
mod_df <- mod_df %>%
  mutate(daily_mileage_cut = cut(daily_mileage, breaks = daily_cuts4))

order <- c("daily_mileage_cut", "acc_inv")
mod <- stages_bhc(full(mod_df[,order]))
plot(mod)

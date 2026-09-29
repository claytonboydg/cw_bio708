## Biostatistics 29 September 2026
## Clayton Glasgow

## Two-Group Comparison -- t-test

# load libraries
library(tidyverse) 

# load data
df_fl <- read_csv("data_src/data_fish_length.csv")
# view data frame
print(df_fl)

# unique returns unique values as a vector
# check which/how many lakes we are comparing
unique(df_fl$lake) # this is a base R function

# 'distinct' returns unique values as a tibble -- same result, different format
distinct(df_fl, lake) # this is a dplyr function

# calculate mean and sd of fish length for each lake
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plots data points with scatter
# geom_segment() draws lines
# geom_point() draws points
df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 3) +
  labs(x = "Lake", # x label
       y = "Fish body length") # y label


## t-test
# lake a lengths
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

# lake b
y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

# run t-test
t.test(x, y, var.equal = TRUE)


# calculate t-value
# make vector of the means
v_mu <- df_fl_mu %>% 
  pull(mu_l)

# calculate difference between means
v_mu[1] - v_mu[2]

# create data frame with lake, mean length, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            var_l = var(length),
            n = n()) # add column with number of samples from each lake

# mean vector
v_mu <- pull(df_t, mu_l)
# variance vector
v_var <- pull(df_t, var_l)
# sample size vector
v_n <- pull(df_t, n)

# pooled variance
var_p <- ((v_n[1] -1) / (sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1) / (sum(v_n) - 2)) * v_var[2]

# calculate t-value
t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

## so t-value gets larger when difference between mean values increases AND/OR
## variance DECREASES

## null hypothesis in t-test is that difference between means is 0

# work to calculate p-value
# generate possible values of t-statistic
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = 98 (Student's t distribution)
y <- dt(x, df = sum(v_n) - 2)
y1 <- dt(x, df = 10 - 2)


# plot student's t distribution
tibble(x, y, y1) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(aes(y = y1),
            color = "red") +
  geom_vline(xintercept = abs(t_value)) + # add vertical lines of abs of t-value
  geom_vline(xintercept = t_value) +
  labs(y = "Probability density",
       x = "t-statistic")

## t-test under unequal variance
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = FALSE)



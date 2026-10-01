## Biostatistics 1 October 2026
## Clayton Glasgow

## two groups laboratory (t-test)

# load libraries
library(tidyverse)


## 10.5.1 Influence of Sample Size

# create vectors
# small samples sizes
xs <- rnorm(n = 10, mean = 10, sd = 5)
ys <- rnorm(n = 10, mean = 12, sd = 5)

# large sample sizes
xl <- rnorm(n = 100, mean = 10, sd = 5)
yl <- rnorm(n = 100, mean = 12, sd = 5)


# perform t-tests
t_test_small <- t.test(xs, ys, var.equal = TRUE)
print(t_test_small) ## not a significant difference (p = 0.2143)

t_test_large <- t.test(xl, yl, var.equal = TRUE)
print(t_test_large) ## significant difference (p = 0.007093)

# degree of freedom is much larger with the larger sample sizes (198 vs. 18)
# p-value is much smaller with the larger sample sizes (0.007 vs. 0.21)

## 10.5.2 Effects of Uncertainty

# create vectors
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

# create tibble from vectors above
df_ab <- tibble(group = rep(c("a1", "a2", "b1", "b2"), each = 10),
                value = c(a1, a2, b1, b2))

# calculate mean & sd for each vector
df_ab_mu <- df_ab %>% 
  group_by(group) %>% 
  summarize(mean = mean(value),
            sd = sd(value))

# create plot similar to fig. 10.1
df_ab %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1,
              height = 0,
              alpha = 0.25) +
  geom_segment(data = df_ab_mu,
               aes(x = group,
                   xend = group,
                   y = mean - sd,
                   yend = mean + sd)) +
  geom_point(data = df_ab_mu,
             aes(x = group,
                 y = mean),
             size = 3) +
  labs(x = "Group",
       y = "Mean (+/- SD)")

# perform t-tests between a1/a2 and b1/b2
t_test_a <- t.test(a1, a2, var.equal = TRUE)
print(t_test_a) # significant difference (p = 2.19 * 10^-8)

t_test_b <- t.test(b1, b2, var.equal = TRUE)
print(t_test_b) # not significant (p = 0.1081)

# all groups have the same sample size, so degree of freedom is the same in
# both t-tests (df = 18)

# the sd is much smaller in the a groups, driving the p-value to be much lower
# than in the b groups, which have much higher sd values

## 10.5.3 Simulate null hypothesis

# load fish length data set
df_fl <- read_csv("data_src/data_fish_length.csv")

# Assign mean(df_fl$length) to mu and sd(df_fl$length) to sig.
mu <- mean(df_fl$length)
sig <- sd(df_fl$length)

# Use rnorm() to generate 50 random observations for each of two groups (x and y) 
# from the same normal distribution, using mean = mu and sd = sig for both groups. 
# This simulates a situation in which you collect 50 observations from each group 
# but with identical means and SDs. Calculate the t-value for this simulated 
# dataset using t.test(x, y, var.equal = TRUE).

x <- rnorm(n = 50, mean = mu, sd = sig)
y <- rnorm(n = 50, mean = mu, sd = sig)

t_test_xy <- t.test(x, y, var.equal = TRUE)
print(t_test_xy) # significant difference (p = 0.0114)

t_test_xy$statistic

# Use a for loop (for (...) { ... }) to repeat this process 100 times, 
# generating a distribution of 100 simulated t-values.

# create empty object
t_statistic <- NULL

# run for loop
for(i in 1:100) {
  
  # generate random distributions with n = 50 and our df_fl mean and sd
  x_i <- rnorm(n = 50, mean = mu, sd = sig)
  y_i <- rnorm(n = 50, mean = mu, sd = sig)
  
  # run t-test
  t_test_i <- t.test(x_i, y_i, var.equal = TRUE)
  
  # save t-statistic to vector
  t_statistic[i] <- t_test_i$statistic
  
}


# Draw a histogram of the simulated t-values and add vertical lines indicating 
# the observed t-value for the comparison of length between lakes a and b in the 
# df_fl dataset. Include vertical lines for both the positive and negative values 
# of the observed t-value to show the two-tailed comparison

# convert t_statistic vector to tibble
t_stat_tib <- tibble(t_statistic)

# plot histogram
t_stat_tib %>% 
  ggplot(aes(x = t_statistic)) +
  geom_histogram(binwidth = 0.5)

# calculate mean & sd for fish length in lakes a & b, run t-test to find t-stat
x_fl <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

# lake b
y_fl <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

# t-test
t_test_fl <- t.test(x_fl, y_fl, var.equal = TRUE)

# identify t statistic for t_test_fl
t_stat_fl <- t_test_fl$statistic

# add positive and negative values of t_stat_fl to histogram
t_stat_tib %>% 
  ggplot(aes(x = t_statistic)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(xintercept = t_stat_fl, color = "red") +
  geom_vline(xintercept = abs(t_stat_fl), color = "red") +
  labs(x = "t-statistic",
       y = "Count")


# Calculate the proportion of simulated t-values whose absolute values are 
# greater than the absolute value of the observed t-value for the difference in  
# length between lakes a and b in the df_fl dataset. Compare this proportion 
# with the p.value obtained from t.test() function.

mean(abs(t_statistic) > abs(t_stat_fl))


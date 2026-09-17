## Biostatistics 17 September 2026
## Clayton Glasgow

## Sampling Laboratory

# load libraries
library(patchwork)
pacman::p_load(tidyverse)

# load dataset
df_h0 <- read_csv("data_src/data_plant_height.csv")

# calculate true mean & variance
mu <- mean(df_h0$height)
sigma2 <- var(df_h0$height) # use unbiased variance var() function

# Sub-datasets of 50 measures ---------------------------------------------

# obtain 100 sub-datasets with 50 measures using for loop
# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# for loop
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 50) # random samples of 50 individuals, assign to df_i
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- var(df_i$height) # use var() for unbiased variance
  
}

# create tibble with sample means and variances
df_sample_50 <- tibble(mu_hat_50 = mu_i, var_hat_50 = var_i)

# plot histogram of sample means & variances
# means
g_mu_50 <- df_sample_50 %>% 
  ggplot(aes(x = mu_hat_50)) +
  geom_histogram() +
  geom_vline(xintercept = mu) + # add true mean as vertical line
  scale_x_continuous(limits = c(15, 25)) +
  xlab("50 sample mean (unbiased)")

# print graph
g_mu_50

# variances
g_var_50 <- df_sample_50 %>% 
  ggplot(aes(x = var_hat_50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits = c(10, 50)) +
  xlab("50 sample variance (unbiased)")

# print graph
g_var_50

# plot the graphs together
g_mu_50 / g_var_50


# Sub-datasets of 100 measures --------------------------------------------

# repeat steps from above, but this time with sample size of 100

# obtain 100 sub-datasets with 100 measures using for loop
# for reproducibility
set.seed(3)

mu_j <- var_j <- NULL # create empty objects

# for loop
for (j in 1:100) {
  
  df_j <- df_h0 %>% 
    sample_n(size = 100) # random samples of 100 individuals, assign to df_j
  
  # save mean for sample set j
  mu_j[j] <- mean(df_j$height)
  
  # save variance for sample set i
  var_j[j] <- var(df_j$height) # use var() for unbiased variance
  
}

# create tibble with sample means and variances
df_sample_100 <- tibble(mu_hat_100 = mu_j, var_hat_100 = var_j)

# plot histogram of sample means & variances
# means
g_mu_100 <- df_sample_100 %>% 
  ggplot(aes(x = mu_hat_100)) +
  geom_histogram() +
  geom_vline(xintercept = mu) + # add true mean as vertical line
  scale_x_continuous(limits = c(15, 25)) +
  xlab("100 sample mean (unbiased)")

# print graph
g_mu_100

# variances
g_var_100 <- df_sample_100 %>% 
  ggplot(aes(x = var_hat_100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) + # add true variance as vertical line
  scale_x_continuous(limits = c(10, 50)) +
  xlab("100 sample variance (unbiased)")
  
# print graph
g_var_100

# plot means and variances graphs together
g_mu_100 / g_var_100

# compare sample sizes
(g_mu_50 / g_mu_100) | (g_var_50 / g_var_100)
## distribution is tighter/more accurate with sample size of 100
## variance of variance is smaller with sample size of 100

# Biased sampling -------------------------------------------------------

# exclude plants < 10 cm
df_h10 <- df_h0 %>% 
  filter(height >= 10)

# repeat steps from section 1 with biased sampling

# obtain 100 sub-datasets with 50 measures using for loop
# for reproducibility
set.seed(3)

mu_b <- var_b <- NULL # create empty objects

# for loop
for (b in 1:100) {
  
  df_b <- df_h10 %>% 
    sample_n(size = 50) # random samples of 50 individuals, assign to df_b
  
  # save mean for sample set i
  mu_b[b] <- mean(df_b$height)
  
  # save variance for sample set b
  var_b[b] <- var(df_b$height) # use var() for unbiased variance
  
}

# create tibble with sample means and variances
df_sample_50_biased <- tibble(mu_hat_50_b = mu_b, var_hat_50_b = var_b)

# plot histogram of sample means & variances
# means
g_mu_50_biased <- df_sample_50_biased %>% 
  ggplot(aes(x = mu_hat_50_b)) +
  geom_histogram() +
  geom_vline(xintercept = mu) + # add true mean as vertical line
  scale_x_continuous(limits = c(15, 25)) +
  xlab("50 sample mean (biased)")
  
# print graph
g_mu_50_biased

# variances
g_var_50_biased <- df_sample_50_biased %>% 
  ggplot(aes(x = var_hat_50_b)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits = c(10, 50)) +
  xlab("50 sample variance (biased)")

# print graph
g_var_50_biased

# plot the graphs together
g_mu_50_biased / g_var_50_biased
# compare to unbiased graphs
(g_mu_50 / g_mu_50_biased) | (g_var_50 / g_var_50_biased)
## mean is far overestimated in the biased sampling regime
## variance is far underestimated in biased sampling regime

# repeat for sub-datasets of 100 measures (biased)

# obtain 100 sub-datasets with 100 measures using for loop
# for reproducibility
set.seed(3)

mu_c <- var_c <- NULL # create empty objects

# for loop
for (c in 1:100) {
  
  df_c <- df_h10 %>% 
    sample_n(size = 100) # random samples of 100 individuals, assign to df_c
  
  # save mean for sample set c
  mu_c[c] <- mean(df_c$height)
  
  # save variance for sample set c
  var_c[c] <- var(df_c$height) # use var() for unbiased variance
  
}

# create tibble with sample means and variances
df_sample_100_biased <- tibble(mu_hat_100_b = mu_c, var_hat_100_b = var_c)

# plot histogram of sample means & variances
# means
g_mu_100_biased <- df_sample_100_biased %>% 
  ggplot(aes(x = mu_hat_100_b)) +
  geom_histogram() +
  geom_vline(xintercept = mu) + # add true mean as vertical line
  scale_x_continuous(limits = c(15, 25)) +
  xlab("100 sample mean (biased)")
  
# print graph
g_mu_100_biased

# variances
g_var_100_biased <- df_sample_100_biased %>% 
  ggplot(aes(x = var_hat_100_b)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits = c(10, 50)) +
  xlab("100 sample variance (biased)")

# print graph
g_var_100_biased

# plot the graphs together
g_mu_100_biased / g_var_100_biased
# compare to unbiased graphs
(g_mu_100 / g_mu_100_biased) | (g_var_100 / g_var_100_biased)
## mean is far overestimated in the biased sampling regime
## variance is far underestimated in biased sampling regime

# test
(g_mu_50 / g_mu_100 / g_mu_50_biased / g_mu_100_biased) | (g_var_50 / g_var_100 / g_var_50_biased / g_var_100_biased)


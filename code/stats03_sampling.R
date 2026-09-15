## Biostatistics 15 September 2026
## Clayton Glasgow

## Sampling

rm(list = ls()) # function to clear/empty global environment

# load libraries
pacman::p_load(tidyverse)


# Individual Samples ------------------------------------------------------


# create tibble
h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit

# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

# create another tibble
h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


# Parameter Inference -----------------------------------------------------


# load csv data on R -- tidyverse version of read_csv, spits out tibble
df_h0 <- read_csv("data_src/data_plant_height.csv")

# show the first 10 rows
print(df_h0)

# calculate true mean & true variance
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)
print(sigma2)

# simulate the sampling of 10 plant individuals by randomly selecting 10 rows
df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)

# obtain 100 sets of 10 plant individuals (randomly selected) and estimate the 
# mean and variance in each -- use a for loop!

# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}

# print outputs
print(mu_i)
print(var_i)

# plot histogram of all sample means

# load patchwork package
library(patchwork)

# create tibble with sample means and variances
df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_mu

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_var

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var

## sample variances are skewed right -- consistently UNDERestimate true variance

## so -- let's redo the simulation with a more unbiased calculation of variance

# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height) # ub = unbiased; this function uses Bessel's correction
}


# draw histograms ---- adding the unbiased variance this time!
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_mu

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_var

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_var_ub

# plot all histograms together
g_mu / g_var / g_var_ub








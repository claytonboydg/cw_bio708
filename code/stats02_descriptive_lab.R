## Descriptive Statistics -- lab
## Biostatistics 10 September 2026
## Clayton Glasgow

# load packages
library(tidyverse)

# Central Tendency --------------------------------------------------------

## exercise 7.3.1
# PRACTICE 1: make vector z & calculate arithmetic mean, geo mean, median
z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))

# calculate arithmetic mean
mu_z <- sum(z) / length(z)

# calculate geometric mean
mu_z_ge <- prod(z)^(1 / length(z))

# calculate median
med_z <- median(z)

# PRACTICE 2: convert to tibble and plot as histogram
# convert z to tibble
tib_z <- tibble(z)
# plot as histogram
tib_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram()

# PRACTICE 3: add vertical lines of mu_z, mu_z_ge, med_z
tib_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z, color = "red") +
  geom_vline(xintercept = mu_z_ge, color = "blue") +
  geom_vline(xintercept = med_z, color = "green")

# PRACTICE 4: Visually compare the values of the central tendency measures with 
# the vertical lines

# median is lowest, geometric mean is slightly less than arithmetic mean

# PRACTICE 5: create new vector z_rev and repeat 1-4
z_rev <- -z + max(z) + 0.5

# calculate arithmetic mean
mu_z_rev <- sum(z_rev) / length(z_rev)

# calculate geometric mean
mu_z_rev_ge <- prod(z_rev)^(1 / length(z_rev))

# calculate median
med_z_rev <- median(z_rev)

# convert z_rev to tibble
tib_z_rev <- tibble(z_rev)
# plot as histogram
tib_z_rev %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram()

# add vertical lines of mu_z, mu_z_ge, med_z
tib_z_rev %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z_rev, color = "red") +
  geom_vline(xintercept = mu_z_rev_ge, color = "blue") +
  geom_vline(xintercept = med_z_rev, color = "green")


# Visually compare the values of the central tendency measures with 
# the vertical lines

# the results are flipped from earlier; median is now highest, and arithmetic
# mean is slightly higher than geometric mean

# Variation ---------------------------------------------------------------

# copied from online textbook
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

# create new vector m that converts w (weight in grams) to milligrams
m <- w * 1000

# calculate sd for m and w
# using built-in r function
sd_w_r <- sd(w)
sd_m_r <- sd(m)

# manually
sd_w_manual <- sqrt(sum((w - mean(w))^2) / length(w))
sd_m_manual <- sqrt(sum((m - mean(m))^2) / length(m))
# manual calculation (1021.8) gives smaller SD than r function (1026.97)

# calculate MAD (median absolute deviation) for w and m
MAD_w <- median(abs(w - median(w)))
MAD_m <- median(abs(m - median(m)))

# calculate CV (coefficient of variation) and MAD/mean for w and m
CV_w <- sd_w_manual / mean(w)
CV_m <- sd_m_manual / mean(m)
# CV_w and CV_m are equal to each other!

MAD_over_mean_w <- MAD_w / median(w)
MAD_over_mean_m <- MAD_m / median(m)
# both MAD/means are equal to each other!




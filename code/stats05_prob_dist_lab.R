## Biostatistics 24 September 2026
## Clayton Glasgow

## Probability Distribution Laboratory

# remove items from previous session
rm(list = ls())

# load libraries
pacman::p_load(tidyverse,
               patchwork)


# Normal Distribution -----------------------------------------------------

## The function rnorm() produces a random variable that follows a Normal 
## distribution with a specified mean and SD. Using this function:

## 1. Generate a variable with 50 observations.
x <- rnorm(50, mean = 10, sd = 2)


## 2. Create a figure similar to Figure 9.3

# calculate mean and sd of x
mu_x <- mean(x)
sigma_x <- sd(x)


# convert x to tibble
x_tib <- tibble(x)

# set bounds & bin for distribution
x_min <- floor(min(x_tib$x)) # floor takes the integer part of the value
x_max <- ceiling(max(x_tib$x)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1

# for loop to calculate estimated number of observations within each bin
p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu_x, sd = sigma_x) - pnorm(bin[i], mean = mu_x, sd = sigma_x)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob_x <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(x_tib))

# generate plot
x_tib %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5,
                 fill = "steelblue") + # specify bin width; must match the bin width used for probability
  geom_point(data = df_prob_x,
             aes(y = freq,
                 x = bin),
             color = "magenta") +
  geom_line(data = df_prob_x,
            aes(y = freq,
                x = bin),
            linetype = "dashed",
            color = "magenta")


# Poisson Distribution ----------------------------------------------------

## The function rpois() produces a random variable that follows a Poisson 
## distribution with a specified mean. Using this function:

## 1. Generate a variable with 1000 observations.
y <- rpois(1000, lambda = 10)

# convert to tibble
y_tib <- tibble(y)

## 2. Create a figure similar to Figure 9.7

# plot initial histogram to see
y_tib %>% 
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.5)


# create vector of x-axis values for poisson distribution
x_axis <- seq(0, 25, by = 1)

# calculate probability mass
lambda_hat <- mean(y_tib$y)
pm <- dpois(x_axis, lambda = lambda_hat)

# plot poisson distribution
tibble(y = pm, x = x_axis) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label


# convert the y-axis from probability to frequency -- multiply the probabilities 
# by the sample size to obtain the expected frequency
df_prob_y <- tibble(x = x_axis, y = pm) %>% 
  mutate(freq = y * nrow(y_tib)) # prob y sample size

# plot together
y_tib %>% 
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.5,
                 fill = "steelblue") +
  geom_line(data = df_prob_y,
            aes(x = x,
                y = freq),
            linetype = "dashed",
            color = "orange") +
  geom_point(data = df_prob_y,
             aes(x = x,
                 y = freq),
             color = "orange")






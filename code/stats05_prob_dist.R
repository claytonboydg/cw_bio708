## Biostatistics 22 September 2026
## Clayton Glasgow

## Probability distribution

# remove items from previous session
rm(list = ls())

# load libraries
pacman::p_load(tidyverse,
               patchwork)

# Continuous variable (probability density function) ----------------------

# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")

# plot height histogram
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

# draw probability distribution
x <- seq(min(df_h0$height),
         max(df_h0$height), 
         length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)


tibble(y = pd,
       x = x) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  labs(y = "Probability density",
       x = "Plant height")

# convert probability density to frequency
# pnorm essentially integrates area below curve, less than whatever 'q' is

# probability of x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma) 
# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)

# probability of 10 < x < 20
p20_10 <- p20 - p10

x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability

for (i in 1:(length(bin)) - 1) {
  ## p_up -- probability up to bin[i + 1]
 p_up <- pnorm(bin[i + 1], mean = mu, sd = sigma)
 ## p_low -- probability up to bin[i]
 p_low <- pnorm(bin[i], mean = mu, sd = sigma)
 
 ## difference p_up - p_low represents probability between bin[i] and bin[i + 1]
 p[i] <- p_up - p_low
  
}

# create dataframe with PDF
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

  
# combine data and PDF
df_h0 %>% 
  ggplot(aes(x = height)) +
  geom_histogram(
    binwidth = 1,
    center = 0.5
  ) +
  geom_point(
    data = df_prob,
    aes(x = bin, y = freq),
    color = "salmon"
  ) + 
  geom_line(
    data = df_prob,
    aes(x = bin, y = freq),
    color = "salmon"
  )
  


# Discrete variability (probability mass function) -----------------------------------------------

# load data set
df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

# histogram
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0)


# Poisson fit

# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0,
                 fill = "steelblue") +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            color = "orange",
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq),
             color = "orange")






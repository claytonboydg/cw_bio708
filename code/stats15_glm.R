## Biostatistics 3 November 2026
## Clayton Glasgow

## generalized linear models

# load libraries
pacman::p_load(tidyverse,
               patchwork)


# poisson -----------------------------------------------------------------



# load data
df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

# visualize data relating nitrate to plant count
df_count %>% 
  ggplot(aes(x = nitrate, y = count)) +
  geom_point()


## run linear model & draw a predicted line
m_lm <- lm(count ~ nitrate, df_count)
summary(m_lm)

df_count %>% 
  ggplot(aes(x = nitrate, y = count)) +
  geom_point() +
  geom_abline(
    intercept = coef(m_lm)[1],
    slope = coef(m_lm)[2],
    color = "magenta"
  )
# predicted line crosses 0, which is impossible for this count data
# but the assumption of normality means assumed normal distributions of error
# around predicted points can cross x-axis

## alternative to ordinary regression
## "Poisson" regression for count data (poisson can only be positive values)

m_pois <- glm(count ~ nitrate, 
              data = df_count,
              family = "poisson")

summary(m_pois)

# add predicted line using glm output
df_count %>% 
  ggplot(aes(x = nitrate, y = count)) +
  geom_point() +
  geom_abline(
    intercept = coef(m_pois)[1],
    slope = coef(m_pois)[2],
    color = "red"
  )
# this figure makes even less sense!
# BECAUSE -- the coefficients are in the ln scale

## link function, log(lambda) = alpha + beta * x for Poisson
x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length = 100)

a <- coef(m_pois)[1] # intercept in a log scale
b <- coef(m_pois)[2] # slope in a log scale

# generate predicted values in log scale
log_y_pred <- a + b * x

# convert predicted values to non-log scale (i.e. regular/actual values)
y_pred <- exp(log_y_pred)

# generate predicted dataset 
df_pred <- tibble(y_pred = y_pred,
                  nitrate = x)

# add proper prediction line to graph
df_count %>% 
  ggplot(aes(x = nitrate, y = count)) +
  geom_point() +
  geom_line(
    data = df_pred,
    aes(y = y_pred)
  )
# line becomes curved

## compare results between lm() and glm()
summary(m_lm)
summary(m_pois)


# binomial ----------------------------------------------------------------


## proportional data, using mussel data
df_mussel <- read_csv("data_src/data_mussel.csv")
print(df_mussel)

# visualize data
df_mussel %>%
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(aes(x = density, y = p)) +
  geom_point() +
  labs(x = "Density of mussels per sq. meter",
       y = "Proportion of eggs fertilized")
 
## binomial regression
## n_examined is the possible maximum success (30)
## n_fertilized is the success numbers
## binomial distribution takes two parameters
## Bin(N, p), where N is the number of trials, p is success probability

# cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

summary(m_binom)
# large reduction from null deviance to residual deviance indicates strong explanatory power of model
# kind of similar to R squared in linear models

# draw prediction
z <- seq(min(df_mussel$density),
         max(df_mussel$density),
         length = 100)

a_binom <- coef(m_binom)[1] # intercept in a logit scale
b_binom <- coef(m_binom)[2] # slope in a logit scale

# generate predicted values in log scale
logit_y_pred <- a_binom + b_binom * z

# convert predicted values to non-log scale (i.e. regular/actual values)
y_pred_binom <- exp(a_binom + b_binom * z) / (1 + exp(a_binom + b_binom * z))

# generate predicted dataset 
df_pred_binom <- tibble(y_pred = y_pred_binom,
                  density = z)


df_mussel %>% 
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(aes(x = density, y = p)) +
  geom_point() +
  geom_line(
    data = df_pred_binom,
    aes(y = y_pred),
    color = "steelblue"
  )









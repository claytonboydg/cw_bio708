## Biostatistics 17 November 2026
## Clayton Glasgow

## model selection

# load libraries
pacman::p_load(tidyverse,
               patchwork)


# model fit ---------------------------------------------------------------

set.seed(1)

# sample size
n <- 100

# true intercept & slope
b <- c(0.1, 0.5)

# hypothetical predictors
x1 <- rnorm(n = n, mean = 0, sd = 1)

# design matrix
X <- model.matrix(~x1)

# simulated y
y_hat <- X %*% b # this weird %*% is matrix multiplication in R
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

df_0 <- tibble(y = y, x1 = x1)

df_0 %>% 
  ggplot(aes(x = x1, y = y)) +
  geom_point()


# lm fit ------------------------------------------------------------------

m1 <- lm(y ~ x1, df_0)
summary(m1)


# adding an unrelated variable to see what happens
df_0 <- df_0 %>% 
  mutate(x2 = rnorm(n = n))

m2 <- lm(y ~ x1 + x2, df_0)
summary(m2)

# extract adjusted R^2 values
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared # adding unrelated variable decreased adj R squared


# non normal case ---------------------------------------------------------

df_count <- read_csv("data_src/data_garden_count.csv") %>% 
  mutate(moisture = runif(nrow(.), 0, 100))


m_pois0 <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

m_pois1 <- glm(count ~ nitrate + moisture,
               data = df_count,
               family = "poisson")

s_pois0 <- summary(m_pois0)
s_pois1 <- summary(m_pois1)

# model0 with nitrate only
pseudoR_0 <- 1 - (s_pois0$deviance / s_pois0$null.deviance)
adj_pseudoR_0 <- 1 - ((s_pois0$deviance - 1) / s_pois0$null.deviance)
# model1 with nitrate + made-up moisture variable
pseudoR_1 <- 1 - (s_pois1$deviance / s_pois1$null.deviance) # this pseudoR is slightly higher
adj_pseudoR_1 <- 1 - ((s_pois1$deviance - 2) / s_pois1$null.deviance) # this adj pseudo R is slightly more than slightly higher than for model0



# likelihood ratio test ---------------------------------------------------

# highly recommended method by Akira

## normal distribution case (m1 and m2 from above)
anova(m1, m2, test = "Chisq") # output gives you model comparison

# if p-value is less than 0.05, then additional variable(s) improves model by more than what would be expected by random chance
# since this value is far above 0.05 -- 0.6253 -- we can say that the second variable isn't needed/useful in the model

## poisson case
anova(m_pois0, m_pois1, test = "Chisq")



# AIC ---------------------------------------------------------------------

## out-of-sample prediction
## AIC = 2k - 2 ln(Likelihood)
## k = # of parameters in your model

# get AIC value from a model
# normal distribution case
AIC(m1)
AIC(m2)
# AIC for m1 is lower, meaning m1 has better capacity to predict out-of-sample data
# in other words, additional variable in m2 doesn't improve model prediction of new data

# poisson case
AIC(m_pois0)
AIC(m_pois1)

# when you see |AIC1 - AIC2| > 2, the model's predictive capacities are meaningfully different
# roughly similar to p-value threshold of 0.05





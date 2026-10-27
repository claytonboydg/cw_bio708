## Biostatistics 27 October 2026
## Clayton Glasgow

## linear models

# load packages
pacman::p_load(tidyverse)



# categorical -------------------------------------------------------------

df_fl <- read_csv("data_src/data_fish_length.csv")

# lm approach
m_fl <- lm(length ~ lake, data = df_fl)
summary(m_fl)

# calculate mean fish length for lakes a & b and find the difference
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

v_mu[2] - v_mu[1] ## 2.056 -- same as regression coefficient from above

## t-test for lake a vs lake b fish length
t_fl <- t.test(length ~ lake, data = df_fl, var.equal = TRUE)

t_fl # t-statistic matches that from the lm above

# anova vs lm
df_anova <- read_csv("data_src/data_fish_length_anova.csv")

# lm with three groups
m_anova <- lm(length ~ lake, data = df_anova)

summary(m_anova)

# calculate mean for each group
v_mu_anova <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

# b minus a
v_mu_anova[2] - v_mu_anova[1] # 2.056 -- same as lake b estimate in lm
# c minus a
v_mu_anova[3] - v_mu_anova[1] # 1.116 -- same as lake c estimate in lm

# do anova
fit_aov <- aov(length ~ lake, data = df_anova)

summary(fit_aov)

# how to modify the order of groups
df_anova <- df_anova %>% 
  mutate(lake_factor = factor(lake, levels = c("c", "a", "b")))

# compare original vs new order
unique(df_anova$lake)
unique(df_anova$lake_factor)

# use lake_factor in lm function
m_fl_factor <- lm(length ~ lake_factor, data = df_anova)

summary(m_fl_factor)


# mix of continuous and categorical in predictors -------------------------

iris <- as_tibble(iris)
distinct(iris, Species) # see how many species in data frame

m_iris <- lm(Petal.Length ~ Petal.Width + Species, data = iris)

summary(m_iris) # categorical variables are effect on intercept


# visualization -----------------------------------------------------------

# generate prediction
n_rep <- 100

df_pred <- iris %>% 
  group_by(Species) %>% 
  reframe(
    Petal.Width = seq(min(Petal.Width), max(Petal.Width), length = n_rep)
  )


y_pred <- predict(m_iris,
                  newdata = df_pred)


df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)


# figure
iris %>% 
  ggplot(aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(
    data = df_pred,
    aes(y = y_pred)
  )


# interaction -------------------------------------------------------------

# manual interaction syntax
# lm(Petal.Length ~ Petal.Width + Species + Petal.Width:Species, # last term adds interaction
#    data = iris)

# short-cut formula; this model is exact same as above
m_iris_int <- lm(Petal.Length ~ Petal.Width * Species,
                 data = iris)

summary(m_iris_int)

y_pred_int <- predict(m_iris_int,
        newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred_int = y_pred_int)


iris %>% 
  ggplot(aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(
    data = df_pred,
    aes(y = y_pred_int)
  )















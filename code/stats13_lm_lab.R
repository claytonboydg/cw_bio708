## Biostatistics 29 October 2026
## Clayton Glasgow

## linear models laboratory

# load packages
pacman::p_load(tidyverse)


# 13.4.1 Normality Assumption ---------------------------------------------


# Using the ToothGrowth dataset,

# 1. Develop a linear model examining the effect of supplement type (supp), 
# dose (dose), and their interaction on tooth length (len). 
# Assign the developed model to m_tooth.

# load tooth growth dataset
tooth <- as.tibble(ToothGrowth)

# linear model
m_tooth <- lm(len ~ supp * dose, data = tooth)



# 2. The Linear Model framework assumes normality. To verify the validity of the 
# normality assumption, we typically employ the Shapiro-Wilk test 
# (shapiro.test() in R). Discuss whether the test should be applied to (1) the response 
# variable or (2) model residuals. Then, apply the Shapiro-Wilk. Type ?shapiro.test 
# in the R console to learn more about its usage.

# test should be applied to the model residuals
# extract model residuals
m_tooth_residuals <- residuals(m_tooth)

# test for normality
shapiro.test(m_tooth_residuals) # W = 0.98, p = 0.5 -- data meets normality assumption


# 13.4.2 Model Interpretation ---------------------------------------------

# Using m_tooth, create a figure illustrating the relationship between tooth length 
# (len; y-axis) and dose (dose; x-axis), with color indicating supplement type (supp). 
# Include both the observed and predicted values, similar to Figure 13.1.

# create new dataset with predicted values based on model; following example in textbook
n_rep <- 100

df_tooth_pred <- tooth %>% 
  group_by(supp) %>% 
  reframe(dose = seq(min(dose),
                    max(dose),
                    length = n_rep))

# make prediction based on supplied values of explanatory variables
y_tooth_pred <- predict(m_tooth,
                  newdata = df_tooth_pred)

# add to df_pred data
df_tooth_pred <- df_tooth_pred %>% 
  mutate(y_tooth_pred = y_tooth_pred)

# generate plot
tooth %>% 
  ggplot(aes(x = dose, y = len, color = supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_tooth_pred,
            aes(y = y_tooth_pred)) + # redefine y values for lines; x and color are inherited from ggplot()
  labs(x = "Dose",
       y = "Tooth Length")



# 13.4.3 Multicollinearity ------------------------------------------------

# Copy and run the following code to create a simulated dataset:

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))


# 1. Create Figures of Response vs. Predictors.

# Using df_y, create the following figures:
#   A scatter plot of y (y-axis) versus x1 (x-axis)
#   A scatter plot of y (y-axis) versus x2 (x-axis)
# Based on these plots, interpret the effects of x1 and x2 on y. 
# Do x1 and x2 appear to have positive or negative influences on y?

# generate plot of x1 and x2 vs y
df_y %>% 
  ggplot(aes(x = x1, y = y)) +
  geom_point(color = "black") +
  geom_point(aes(x = x2), color = "red") +
  labs(x = "x (x1 = black, x2 = red)",
       y = "y")


## both x1 and x2 appear to have positive influences on y


# 2. Examining statistical effects of x1 and x2 on y.
# Examine the statistical effects of x1 and x2 on y using the lm() function.
# Suppose that x1 and x2 are factors that vary across samples. 
# In this case, you want to account for the influence of one variable while assessing 
# the effect of the other. To do so, include both predictors in a single linear 
# model using the + operator. 
# Discuss whether the statistical estimates of x1 and x2’s effects make sense.

m_y <- lm(y ~ x1 + x2, data = df_y)

summary(m_y)

# the intercept (p = 3.78e-14) and slope for x2 (4.61e-06) are significant
# x1 slope estimate is slightly negative (but not significant, according to the model)
# this result doesn't make sense given that x1 clearly has a positive relationship
# with y when looking at the graph

# 3. Create a Figure of Predictor vs. Predictor.
# Using df_y,
#   Create a scatter plot of x1 and x2.
#   Examine the Pearson’s correlation between x1 and x2 using a function cor(). 
#     (?cor() for its usage).

# plot x1 vs. x2
df_y %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point()

# the points appear highly correlated (more tightly correlated with each other than either is with y)

# calculate Pearson's correlation between x1 and x2
cor_x1_x2 <- cor(df_y$x1, df_y$x2, method = "pearson")

print(cor_x1_x2) # Pearson's correlation = 0.88 -- high correlation


# 13.4.4 Experimental Design ----------------------------------------------

# I think choice B is the most appropriate statement regarding the proposed
# experimental design ("This experiment can capture the effects of nutrient and 
# predation. However, the nutrient effect may be limited to the presence of predators, 
# and the predator effect is limited to the high nutrient condition.)

# Because the design has treatments of high nutrients/predator present and 
# low nutrients/predator present, the experiment can assess the effects of nutrients,
# but only in the presence of predators. Similarly, the treatment of high/absent
# allows for assessment of predators, but only at high levels of nutrients.
# However, the lack of a fully factorial experiment (i.e. no low/absent treatment)
# prevents assessment of the interaction term.










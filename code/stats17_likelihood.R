## Biostatistics 10 November 2026
## Clayton Glasgow

## likelihood

# load libraries
pacman::p_load(tidyverse,
               patchwork)


# poisson example for likelihood ------------------------------------------

# probability of observing 3 given lambda 3.5
(pr3 <- dpois(3, lambda = 3.5))
# 3 = calculating probability that value in given pois distribution is 3
# lambda is the mean of the given data set that the pois distribution is based on
# output is 0.2157855, which is the probability that, in a pois distribution, there is a value of 3 if lambda = 3.5

# however, we don't know if 3.5 is the best/reasonable lambda for observing a value of 3
# SO -- try over many different values
# try 0.1-10 as possible values of lambda (intervals of 0.1)

# create vector
lambda <- seq(0.1, 10, by = 0.1)
# calculate probability for each value in vector
pr <- dpois(3, lambda = lambda)

# create dataset with these values
df_pois <- tibble(y = 3,
                  lambda = lambda,
                  pr = pr)

# see which lambda maximizes probability of observing 3
df_pois %>% 
  arrange(desc(pr))
# pr(k = 3) is highest when lambda = 3, surprise surprise

# plot data
df_pois %>% 
  ggplot(aes(x = lambda, y = pr)) +
  geom_point() +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(k = 3)")


## vector case y = {3, 2, 5}
## probability of observing 3 data points together
## use lambda = 3.5
y <- c(3, 2, 5)

pr_vector <- dpois(x = y, lambda = 3.5)
# output gives you vector of same length as y with probabilities that value is 3, 2, and 5, respectively

# so to calculate probability that these three values are all observed together, you multiply these probabilities together
# manual calculation
lik <- pr_vector[1] * pr_vector[2] * pr_vector[3]
print(lik) # very small number! 5.488e-10

# same thing but with built-in function
prod(pr_vector)


# same as before, find lambda that maximizes probability of observing this vector
# likelihood for each lambda value 0.1-10; probability of observing vector y(3,2,5)
lambda_vec <- seq(0.01, 10, by = 0.01)
lik_i <- sapply(lambda_vec,
                function(x) {
                  prod(dpois(y, lambda = x))
       })

# store output in dataset
df_pois_vec <- tibble(lambda = lambda_vec, 
                      lik = lik_i) %>% 
  arrange(desc(lik))

# max likelihood value of lambda matches sample mean of y
mean(y) # equals 3.33, which is also max likelihood


## why sample mean matches the MLA (Maximum Likelihood Estimate)
## Pr(y = 3) with lambda = 3.5
dpois(3, lambda = 3.5)
(3.5^3 * exp(-3.5)) / factorial(3)
# these give you the same number; the second is the manual calculation of the dpois function

## visualiztion
df_pois_vec %>% 
  ggplot(aes(x = lambda, y = log(lik))) +
  geom_line() +
  labs(x = "lambda value",
       y = "likelihood")


## get likelihood from glm
# load garden plant data
df_count <- read_csv("data_src/data_garden_count.csv")

# run glm
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)
summary(m_pois)


















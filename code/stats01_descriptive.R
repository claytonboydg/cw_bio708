## Biostatistics 8 September 2026
## Clayton Glasgow

# Descriptive Statistics

# call packages every time
library(tidyverse)


# Central Tendency --------------------------------------------------------


# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

# arithmetic mean -- always larger than geometric; more sensitive to outliers
mu_x <- sum(x) / length(x)
mean(x)

# adding parentheses prints the value in addition to saving it to object
(mu_y <- sum(y) / length(y))
mean(y)

# geometric mean -- always less than arithmetic mean
prod(x) # multiply all elements of vector x together

mu_x_ge <- prod(x)^(1 / length(x)) # use ^ to raise to a power

# using logarithm
exp(sum(log(x)) / length(x))

# for vector y
mu_y_ge <- prod(y)^(1 / length(y))
exp(sum(log(y)) / length(y))


# median
med_x <- median(x)
med_y <- median(y)



# Variation ---------------------------------------------------------------

# variance
var_x <- sum((x - mean(x))^2) / length(x)

var_y <- sum((y - mean(y))^2) / length(y)

sqrt(var_x)
sqrt(var_y)

# inter-quantile range (IQR)
x_l <- quantile(x, 0.25)
x_h <- quantile(x, 0.75)

(iqr_x <- x_h - x_l)


y_q <- quantile(y, c(0.25, 0.75))

iqr_y <- y_q[2] - y_q[1]


# MAD ---------------------------------------------------------------------

# MAD -- median absolute deviation
ad_x <- abs(x - median(x)) # abs takes the absolute value
median(ad_x)

ad_y <- abs(y - median(y))
median(ad_y)



# Coefficient of Variation ------------------------------------------------

sd_x <- sqrt(var_x)
cv_x <- sd_x / mu_x



# Extra -------------------------------------------------------------------

my_cv <- function(v) {
 
   mu <- mean(v)
  s <- sd(v)
  cv <- s / mu
  
  return(cv)
}

my_cv(x)

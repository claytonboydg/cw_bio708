## Biostatistics 12 November 2026
## Clayton Glasgow

## likelihood lab

# load libraries
pacman::p_load(tidyverse,
               patchwork)



# 15.3.1 Binomial Distribution --------------------------------------------

# create vector y
y <- c(2,2,0,0,3,1,3,3,4,3)

# create vector p
p <- seq(0, 1, by = 0.01)

# set N
N <- 10


# calculate likelihood of observing vector y for the values of p
db_lik <- sapply(p, function(x){
  prod(dbinom(y, size = N, prob = x))
})

# convert to tibble & arrange by descending likelihood to identify parameter value that maximizes likelihood
df_lik_db <- tibble(p = p, likelihood = db_lik) %>% arrange((desc(likelihood)))
# p = 0.21 maximizes likelihood


# Using p that maximizes likelihood, calculate N * p and compare to sample mean
N * 0.21 # 2.1, because N = 10

mean(y) # also 2.1


# 15.3.2 Normal Distribution ----------------------------------------------

# this part is just math, I think. do on a separate piece of paper





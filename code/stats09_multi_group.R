## Biostatistics 6 October 2026
## Clayton Glasgow

## Multi-Group Comparison (ANOVA)

# load packages
library(tidyverse)

# load data
df_anova <- read_csv("data_src/data_fish_length_anova.csv")
# show how many different lakes are in data set
distinct(df_anova, lake) 

# plot data as violin plot
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency

## ANOVA compares the between-group variation to the within-group variation
## If between-group variability is greater than within-group variability,
## then it is likely there are significant differences among the groups

## to calculate within-group and between-group variation, we need to calculate
## the overall mean length of all fish (from all lakes) and the mean length of
## fish in each separate lake

# estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_g)

# sum dev_g column to get variation within each lake
df_g <- df_g %>% 
  mutate(ss = dev_g * n)

print(df_g)

# Sum over g (lake) to get Sb (between-group variability)
s_b <- sum(df_g$ss)
print(s_b)


## similar process for within-group variation
df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

# look at group-level data
# filter() & slice(): show first 3 rows each group
print(df_i %>% filter(lake == "a") %>% slice(1:3))
print(df_i %>% filter(lake == "b") %>% slice(1:3))
print(df_i %>% filter(lake == "c") %>% slice(1:3))

# sum deviations for each lake, and then sum across all lakes to get
# within-group variation

# sum deviations for each lake
df_i_g <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))

print(df_i_g)

# sum ss across all lakes
s_w <- sum(df_i_g$ss)
print(s_w)


## so far, Sb (between-group) and Sw (within-group) are “variability,” 
## which essentially represents the summation of squared deviations

## To convert them into variances, we can divide them by appropriate numbers. 
## In Chapter 8, I mentioned that the denominator for variance is the sample 
## size minus one. The same principle applies here, but with caution.

# For Sb, the realized sample size is the number of groups -- 3 different lakes
# Therefore, we divide by three minus one to obtain an unbiased estimate of the 
# between-group variance

# n_distinct() count the number of unique elements
n_g <- n_distinct(df_anova$lake)
# calculate between-group variance
s2_b <- s_b / (n_g - 1)
print(s2_b)


# Meanwhile, we need to be careful when estimating the within-group variance. 
# Since the within-group variance is measured at the individual level, the 
# number of data used is equal to the number of fish individuals. Yet, we 
# subtract the number of groups – while the rationale behind this is beyond the
# scope, we are essentially accounting for the fact that some of the degrees of 
# freedom are “used up” in estimating the group means

# calculate within-group variance
s2_w <- s_w / (nrow(df_anova) - n_g)
print(s2_w)

## use between- and within-group VARIANCE to calculate the F-statistic
f_value <- s2_b / s2_w
print(f_value)
# f-value = 5.3 -- indicates that the between-group variance is approximately 
# five times higher than the within-group variance

# generate F-distribution
# The F-statistic follows an F-distribution when there is no difference in 
# means among the groups

# generate x range
x <- seq(0, 10, by = 0.1)


# generate F-distribution
# The degrees of freedom in an F-distribution are determined by two parameters:
# df1 = Ng (number of groups being compared, for this case 3) - 1
# df2 = N (total individuals, for us 150) - Ng (number of groups)
y <- df(x = x, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)

# plot F-distribution & add our f-statistic as a vertical line
tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value,
             color = "salmon") # observed F-statistic

## Unlike t-statistics, F-statistics can take only positive values 
## (because F-statistics are the ratio of positive values).

## The p-value here is Pr(F0 > F), where F0 is the possible F-statistics under 
## the null hypothesis

## we can estimate that probability using the pf() function
# Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)


## using built-in R functions for ANOVA
# first argument is formula
# second argument is data frame for reference
# do not forget specify data = XXX! aov() refer to columns in the data frame
m <- aov(formula = length ~ lake,
         data = df_anova)

print(m)

# use summary to get p-value
summary(m)









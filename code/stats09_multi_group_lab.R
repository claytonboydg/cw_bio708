## Biostatistics 8 October 2026
## Clayton Glasgow

## ANOVA Laboratory

# load library
library(tidyverse)


# 11.5.1 -- Application to PlantGrowth ------------------------------------

## 1. The data set consists of two columns: weight, group. 
## Create figures similar to Figure 11.1.

# load plant data
df_plants <- PlantGrowth

# plot similar to Figure 11.1
df_plants %>% 
  ggplot(aes(x = group, y = weight, color = group)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.6) + # transparency
  geom_jitter(alpha = 0.6) # transparency


## 2. Conduct an ANOVA to examine whether there are differences in weight among 
## the different groups.
plant_weight_anova <- aov(weight ~ group, data = df_plants)

print(plant_weight_anova)
summary(plant_weight_anova)

## 3. Discuss what values to be reported in a scientific article.

# in a scientific paper, I would report the following values:

  # p-value -- 0.0159, indicating that there is at least one significant 
  # difference among the groups

  # F-statistic -- 4.846, which shows the ratio of between-group to within-group
  # variance. A value of 4.846 means that the between-group variance was nearly
  # 5 times greater than the within group variance

  # Degrees of freedom -- there are two numbers to report here. The df for 
  # between-groups is 2 (Ng - 1, with Ng = 3), and the df for within-groups is 27 
  # (N - Ng, with N = 30 and Ng = 3)


# ## 11.5.2 Power analysis ------------------------------------------------

# You’re planning to compare the mean plant biomass among three different 
# wetlands using ANOVA. You expect a large effect size (Cohen’s f = 0.5) and 
# want to achieve 80% power at a 0.05 significance level. 

# How can you calculate the required sample size per group in R? 
# You may use pwr::pwr.anova.test() function in R.

# install pwr package
# install.packages("pwr")

pwr::pwr.anova.test(
  k = 3,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8
)

# required sample size is 13.89521, so 14 samples per group would need to be used





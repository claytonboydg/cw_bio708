## Biostatistics 5 November 2026
## Clayton Glasgow

## generalized linear models lab

# load libraries
pacman::p_load(tidyverse,
               patchwork)


# 14.4.1 GLM exercise -----------------------------------------------------

## 1. Fish example

# load data
url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

# visualize data
df_fish %>% 
  ggplot(aes(x = distance, y = n_sp)) +
  geom_point()

df_fish %>% 
  ggplot(aes(x = cat_area, y = n_sp)) +
  geom_point()

df_fish %>% 
  ggplot(aes(x = hull_area, y = n_sp)) +
  geom_point()

# run glm with poisson distribution (discrete data, can't be less than 0)
species_glm <- glm(n_sp ~ distance + cat_area + hull_area,
                   data = df_fish,
                   family = "poisson")

summary(species_glm)
# distance is the only predictor that has a significant effect (p = 0.0364)


## 2. Car example

# load data
mtcars <- as_tibble(mtcars)

# visualize data
mtcars %>% 
  ggplot(aes(x = mpg, y = am)) +
  geom_point()

mtcars %>% 
  ggplot(aes(x = hp, y = am)) +
  geom_point()

mtcars %>% 
  ggplot(aes(x = wt, y = am)) +
  geom_point()

# run glm using binomial family
am_glm <- glm(am ~ mpg + hp + wt,
              data = mtcars,
              family = "binomial")

summary(am_glm)
# car weight is the strongest predictor 


# run glm using normal distribution
am_glm_normal <- glm(am ~ mpg + hp + wt,
                     data = mtcars,
                     family = "gaussian")

summary(am_glm_normal)
# here, both wt AND hp are significant; this is why choosing right error structure matters


# 14.4.2 Effect size ------------------------------------------------------

# The function scale() will perform standardization. 
# Create columns of standardized distance (std_dist), cat_area (std_cat), and hull_area (std_hull).

df_fish <- df_fish %>% 
  mutate(
    std_dist = c(scale(distance)),
    std_cat = c(scale(cat_area)),
    std_hull = c(scale(hull_area))
  )

# Perform a GLM analysis of fish species richness with the standardized variables, 
# and identify the most influential variable among them in terms of effect size.

species_std_glm <- glm(n_sp ~ std_dist + std_cat + std_hull,
                       data = df_fish,
                       family = "poisson")

summary(species_std_glm)
# distance is still the only predictor with a significant effect
# BUT -- the estimates are much larger (for example, std_dist = -0.17 vs distance = -1.762e-05)
# scaling doesn't change significance/effect, but will make output values (slopes/intercept) more interpretable


# 14.4.3 Offset term ------------------------------------------------------

# load data
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

# visualize data
df_offset %>% 
  ggplot(aes(x = nitrate, y = count)) +
  geom_point(color = "orange")

df_offset %>% 
  ggplot(aes(x = area, y = count)) +
  geom_point(color = "blue")

df_offset %>% 
  ggplot(aes(x = nitrate, y = count/area)) +
  geom_point(color = "magenta")

# confirm that code below returns an error
# df_offset <- df_offset %>% 
#   mutate(density = count / area)
# 
# glm(density ~ nitrate,
#     data = df_offset,
#     family = "poisson")
# yes indeed -- "There were 50 or more warnings"


# A better approach is to incorporate sampling effort as an offset term in glm(), 
# which allows the model to account for differences in effort while preserving the 
# count nature of the response variable. Perform the following exercise:
#   

# Develop a GLM with the response variable count and the predictor nitrate.
count_nit_glm <- glm(count ~ nitrate,
                     data = df_offset,
                     family = "poisson")

summary(count_nit_glm)

# Develop a GLM with the response variable count, the predictor nitrate, 
# and the offset term offset(log(area)) (~ nitrate + offset(log(area)) will work).
count_nit_offset_glm <- glm(count ~ nitrate,
                            data = df_offset,
                            offset = offset(log(area)),
                            family = "poisson")

summary(count_nit_offset_glm)
# Compare the results of the two models.
# p-value is far lower/more significant in the offset model than non-offset model

# 14.4.4 Overdispersion ---------------------------------------------------

# load data
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)

# Plot the relationships between (a) tadpole and aqveg and (b) tadpole and permanence.
df_tadpole %>% 
  ggplot(aes(x = aqveg, y = tadpole)) +
  geom_point(color = "darkgreen")

df_tadpole %>% 
  ggplot(aes(x = permanence, y = tadpole)) +
  geom_point(color = "darkblue")

# prediction: permanence has more significant effect than aquative vegetation

# develop GLM
tadpole_glm <- glm(tadpole ~ permanence + aqveg,
                   data = df_tadpole,
                   family = "poisson")

summary(tadpole_glm)
# both are aqveg and permanence are significant, though the p-value for permanence
# is much lower (9.29e-08) than the p-value for aqveg (0.0354)

# however, the slope estimate for aqveg is far higher (0.47) than that of aqveg (0.029)



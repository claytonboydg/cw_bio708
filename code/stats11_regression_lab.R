## Biostatistics 22 October 2026
## Clayton Glasgow

## regression laboratory

# load library
library(tidyverse)


# 12.4.1 Develop regression models ----------------------------------------


# R provides a built-in data set called iris. The iris data contain data points 
# from three different species (Species column). Split this data set by species 
# (create three separate data frames) and perform regression for each species 
# separately to analyze the relationship between Sepal.Width (response variable) 
# and Petal.Width (explanatory variable).

# load data
df_iris <- iris


# visualize data
df_iris %>% 
  ggplot(aes(x = Sepal.Width, y = Petal.Width, color = Species)) +
  geom_point() +
  facet_wrap("Species") # facet by species


## create new dataset for each individual species &  perform regression

# create vector of species names
iris_species <- c("setosa", "versicolor", "virginica")

# create empty list to store model output
model_list <- list()

# for loop
for(i in iris_species){
  
  # create dataset for each individual species
  df_subset <- iris %>% filter(Species == i)
  
  # perform linear model on df_subset & save to model list
  model_list[[i]] <- lm(Petal.Width ~ Sepal.Width, data = df_subset)
  
}

# summarize model outputs
summary(model_list$setosa) # not significant slope -- p = 0.104
summary(model_list$versicolor) # significant slope -- p = 1.47e-07
summary(model_list$virginica) # significant slope -- p = 1.467e-07


## other alternative w lapply() -- lapply stands for 'least apply'

# lapply(x = iris_species,
#        function(s) {
#        iris %>%  filter(Species == s) %>% 
#        lm(Sepal.Width ~ Petal.Width, data = .)
#  })

# 12.4.2 Multiple explanatory variables
# Regression analysis can involve multiple explanatory variables. To explore this,
# consider utilizing Petal.Length as an additional explanatory variable for 
# each species. Then, investigate (1) the variations in estimates of regression 
# coefficients and (2) the differences in the coefficients of determination 
# compared to the model with only a single explanatory variable.

# visualize data
df_iris %>% 
  ggplot(aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  facet_wrap("Species") # facet by species

# same for loop as above, but adapted for additional species
# new model list
model_list_2 <- list()

# for loop
for(i in iris_species){
  
  # subset data by species
  df_subset_2 <- iris %>% filter(Species == i)
  
  # run lm models & save to list
  model_list_2[[i]] <- lm(Petal.Width ~ Sepal.Width + Petal.Length, data = df_subset_2)
}

# summarize model outputs & compare
summary(model_list_2$setosa) # significant slope for Petal.Length -- p = 0.0341
# R squared increased; slope for sepal.width slope decreased

summary(model_list_2$versicolor) # significant slopes for Sepal.Width (p = 0.0017) and Petal.Length (p = 1.45e-07)
# R squared increased; slope for sepal.width slope decreased

summary(model_list_2$virginica) # significant slope for Sepal.Width -- p = 0.000635
# Multiple R squared increased slightly. but adjusted R squared decreased slightly
# sepal.width slope decreased slightly; slope for petal length was very small and not significant














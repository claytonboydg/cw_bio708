## Biostatistics 19 November 2026
## Clayton Glasgow

## model selection laboratory

# load libraries
pacman::p_load(tidyverse,
               patchwork)

# install.packages("palmerpenguins")
library(palmerpenguins)

# install.packages("janitor")
library(janitor)

# 16.3.1 Format Penguin Data ----------------------------------------------

# load data
penguins <- penguins_raw

# fix column names
penguins <- clean_names(penguins)

# convert clutch completion to 1 (yes) or 0 (no)
penguins <- penguins %>% 
  mutate(clutch_completion = ifelse(penguins$clutch_completion == "Yes", 1, 0))

# convert species names to one word (adelie, gentoo, chinstrap)
penguins <- penguins %>% 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"
  ))

## better way to have done the above would have been to start by doing:
## pen_species <- unique(penguins$species)
## then -- within case_when function above, could do species == pen_species[1] ~ "adelie", etc.

## also could have combined with previous mutate of clutch_completion

# remove rows with NA values
penguins <- drop_na(penguins, c("culmen_length_mm", 
                                "culmen_depth_mm", 
                                "flipper_length_mm", 
                                "body_mass_g", "sex"))


# 16.3.2 Analyze Penguin Data ---------------------------------------------

# Develop a statistical model that explains clutch completion as function of
# species, culmen length, culmen depth, flipper length, body mass, sex

# visualize data
# completion by species
penguins %>% 
  ggplot(aes(x = species, y = clutch_completion, color = species)) +
  geom_jitter(width = 0.15, height = 0.04, alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 4) +
  scale_y_continuous(breaks = c(0, 1))

# maybe chinstrap penguins have lower rate of clutch completion, but all three
# species seem pretty similar

# run full model; will later choose best model using dredge (this is just a starting point that includes everyting)
m_clutch <- glm(clutch_completion ~ species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + body_mass_g + sex,
                data = penguins,
                family = "binomial")

summary(m_clutch)


# Perform an AIC-based model selection using the MuMIn::dredge() function
# This process will help us identify the variables that are most important for 
# predicting the clutch completion status.

# from online textbook
# install.packages("MuMIn")
library(MuMIn)
options(na.action = "na.fail")

# use dredge function to rank all possible models by AIC
m <- m_clutch
m_set <- dredge(m, rank = "AIC")
m_sub <- subset(m_set, delta < 2) 
# models with delta AIC less than 2 are generally considered competitive/comparable 
# to the best model; delta AIC greater than considered 'significantly' worse model
# (kind of like p < 0.05)

list_m <- get.models(object = m_set, subset = delta < 2)
list_m[[1]] # accesses first element of list_m, which is output of first model in the list

# looks like best model only includes body mass & culmen length
# visualize data
penguins %>% 
  ggplot(aes(x = body_mass_g, y = clutch_completion, color = culmen_length_mm)) +
  geom_point()

penguins %>% 
  ggplot(aes(x = body_mass_g, y = culmen_length_mm, color = species)) +
  geom_point()


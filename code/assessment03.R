## Biostatistics 24 November 2026
## Clayton Glasgow
## Assessment 3

# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

library(tidyverse)
library(janitor)
library(patchwork)
library(MuMIn)
options(na.action = "na.fail") # set for MuMIn package

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`
df_co2 <- tibble(CO2)

# Q2
# Convert column names to lowercase and reassign to `df_co2`
df_co2 <- clean_names(df_co2)

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# The figure should satify the following:
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

# quebec plot
quebec_plot <- df_co2 %>% filter(type == "Quebec") %>% 
  ggplot(aes(x = conc, y = uptake, color = treatment)) +
  geom_point() +
  labs(x = "ambient [CO2] (ml/L)",
       y = "CO2 assimilation (µmol/m²/sec)",
       title = "CO2 assimilation vs ambient [CO2] in Quebec (top) and Mississippi (bottom)")

# mississippi plot
ms_plot <- df_co2 %>% filter(type == "Mississippi") %>% 
  ggplot(aes(x = conc, y = uptake, color = treatment)) +
  geom_point() +
  labs(x = "ambient [CO2] (ml/L)",
       y = "CO2 assimilation (µmol/m²/sec)")

# combine plots in separate panels
quebec_plot / ms_plot

# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop a statistical model that examines the following effects simultaneously:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

## Quebec model
df_quebec <- df_co2 %>% filter(type == "Quebec")

m_quebec <- lm(uptake ~ conc * treatment,
               data = df_quebec)

summary(m_quebec) # significant intercept & coefficient for ambient CO2

## Mississippi model
df_ms <- df_co2 %>% filter(type == "Mississippi")

m_ms <- lm(uptake ~ conc * treatment,
           data = df_ms)

summary(m_ms) # significant intercept & coefficients for all predictors and their interaction

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:
# (no coding required for this question)

# For plants from Quebec, CO2 assimilation increased as ambient CO2 concentrations increased (to a point),
# as demonstrated by the significant effect of 'conc' in the Quebec model (p = 9.43e-12). 
# There was no effect of the treatment (chilled vs non-chilled), as demonstrated by a lack of
# significant coefficients for both the treatment and the treatment x concentration interaction.
# In the figure, the chilled Quebec plants (teal-colored points) appear to have slightly lower CO2 uptake rates as
# compared to the non-chilled plants (salmon-colored points), but evidently the difference is small and not statistically
# significant in this model.

# Mississippi plants showed a similar qualitative trend, with increasing CO2 assimilation as
# ambient CO2 concentrations increased (to a point) (significant effect of 'conc' -- p = 1.02e-12).
# However, in the Mississippi data, there was also a significant effect of treatment (p = 0.0233)
# and a significant interaction between conc and treatment (p = 0.0482). As can be seen in the
# figure (bottom panel), the chilled and non-chilled treatments clearly diverge, with the 
# non-chilled plants (salmon-colored points) assimilating CO2 at a significantly faster rate 
# than the chilled plants (teal-colored points).

# From an ecological perspective, this result isn't surprising. I would expect plants in Quebec
# to be more adapted to chilly conditions, whereas Mississippi plants probably aren't so
# adapted to colder temperatures.

# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`
df_env <- clean_names(df_env)

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

df_env_sub <- remove_constant(df_env)

# Q8
# Calculate summary statistics for each plot using `df_bci`.
# Compute the following quantities by plot as columns:
# - n_sum: total count of all individuals across species
# - n1: count of the most abundant species (maximum count among species for each plot)
# - p: proportion of the most abundant species (n1 / n_sum)
#
# Ensure that each plot has one row representing a data point of the dominant species.
#
# Assign the resulting dataframe to `df_n`.

df_n <- df_bci %>% 
  group_by(plot) %>% 
  summarize(n_sum = sum(count),
             n1 = max(count),
             p = n1 / n_sum)

# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- merge(df_env_sub, df_n, by = "plot")

# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species with a suitable error distribution. 
# - Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# - Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

# run full model; use cbind to combine 'successes' (dominant species) and 'failures' (# of individuals not in dominant species)
m_binom <- glm(cbind(n1, n_sum - n1) ~ env_het + stream + habitat,
               data = df_m,
               family = "binomial")

summary(m_binom)

# select best model based on AIC (out-of-sample prediction)
m_select <- dredge(m_binom, rank = "AIC")
m_subset <- subset(m_select, delta < 2) # identify top models

m_subset
# BEST MODEL includes environmental heterogeneity and habitat as predictors
# best model does not include stream, but the second best model which has a delta of 2,
# indicating that it is comparable in strength to the top model, does include stream
# in addition to environmental heterogeneity and habitat




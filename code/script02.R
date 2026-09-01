# Biostats 8.25.26
# Clayton Glasgow

# install and load packages
# install.packages("tidyverse")
library(tidyverse)

# set up from online textbook
set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

## FILTER

# filter to only virginica species
filter(iris_sub, Species == "virginica")
# filter to all species EXCEPT virginica
filter(iris_sub, Species != "virginica")
# filter based on sepal width
filter(iris_sub, Sepal.Width >= 4)

# filter to species specified in a vector
filter(iris_sub, Species %in% c("virginica", "versicolor"))
# same idea, | symbol means 'or'
filter(iris_sub, Species == "virginica" | Species == "versicolor")
# can use 'or' with different columns, unlike with %in%
filter(iris_sub, Sepal.Width > 5 | Species == "versicolor")


## ARRANGE

# arrange via ascending sepal length -- ascending is the default
arrange(iris_sub, Sepal.Length)
# arrange descending
arrange(iris_sub, desc(Sepal.Length))

## 5.1.3 EXERCISE
iris_3 <- filter(iris_sub, Sepal.Width > 3)

iris_setosa <- filter(iris_sub, Species == "setosa")

iris_3_setosa <- filter(iris_sub, Sepal.Width > 3 & Species == "setosa")


## SELECT

# select sepal length column
select(iris_sub, Sepal.Length)
# select multiple columns using a vector
select(iris_sub, c("Sepal.Length", "Sepal.Width"))

# select which columns to NOT include using minus sign
select(iris_sub, -Sepal.Width) # prints all columns EXCEPT sepal width

# select based on start of column
select(iris_sub, starts_with("Sepal")) # prints columns that start with Sepal
# same idea, but end of column name
select(iris_sub, ends_with("Width")) # prints columns that end with Width

## MUTATE -- adding columns

# set up exercise
x_max <- nrow(iris_sub)
# make vector from 1 to x max
x <- 1:x_max

# add new column to iris sub using x
mutate(iris_sub, row_id = x) # "row_id" is the name of new column, x the data to fill column

# add column that doubles sepal length
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

# add column that merges text and numbers to create unique ID for each row
mutate(iris_sub, id_name = paste(Species, Sepal.Length)) # paste fxn stitches inputs together


## 5.2.3 EXERCISE
iris_pw <- select(iris_sub, c("Petal.Width", "Species"))

iris_petal <- select(iris_sub, starts_with("Petal"))

iris_pw_two <- mutate(iris_sub, pw_two_times = 2 * Petal.Width)

## PIPING

# without piping
df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

print(df_vir_sl)

# with piping 
iris_sub %>%
  filter(Species == "virginica") %>%
  select(Sepal.Length)


## 5.3.2 EXERCISE
iris_pipe <- iris_sub %>% 
  filter(Species == "setosa") %>% 
  mutate(pw_two_times = 2 * Petal.Width)


## GROUPING
print(iris_sub)

# group by species; dataframe doesn't change, but now has three groups
iris_sub %>% group_by(Species)

# group by species, then calculate avg sepal length for each species
iris_sub %>%
  group_by(Species) %>%
  summarize(mu_sl = mean(Sepal.Length))

# calculate multiple summary statistics for each species
iris_sub %>%
  group_by(Species) %>%
  summarize(mu_sl = mean(Sepal.Length),
            sum_sl = sum(Sepal.Length))

# grouping by "Species", then take means "Speal.Length" for each species
# and add those means to new column
iris_sub %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup() # need to add to avoid errors; ungroups so that each row has the avg corresponding to its species





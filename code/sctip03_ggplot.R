## Biostats 9.1.26
## Clayton Glasgow

# call every time
library(tidyverse)


# Point Figure ------------------------------------------------------------
# Cmd + Shift + R inserts section title (see above)
# table of contents pops up (click lines to the right of "Source" at top right)
## point figure
iris %>%
  ggplot(
    mapping = aes(x = Sepal.Length,
                  y = Sepal.Width)
  ) +
  geom_point()

## short cut for commenting out (or un-commenting), Cmd + Shift + C 
# this produces an error
# iris %>% 
#   ggplot(
#     mapping = x = Sepal.Length,
#               y = Sepal.Width)
#   ) + 
#   geom_point()

## color by species
# example without pipe
ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width,
                color = Species)
) +
  geom_point()

## this returns an error -- color arg must be inside aes()
# ggplot(
#   data = iris,
#   mapping = aes(x = Sepal.Length,
#                 y = Sepal.Width),
#   color = Species
# ) +
#   geom_point()

## color all points 'steelblue'
# this arg must go in geom_point
ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width,
                color = Species)
) +
  geom_point(color = "steelblue")



# Line Figure -------------------------------------------------------------


# sample data
df0 <- tibble(
  x = rep(1:50, 3),
  y = x * 2
)

# plot x vs y on line graph
df0 %>% 
  ggplot(
    mapping = aes(x = x,
                  y = y)
  ) + 
  geom_line()



# Histogram ---------------------------------------------------------------
# plot histogram (only use 1 variable)
iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram()



# Boxplot -----------------------------------------------------------------

# generate boxplot
iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length)
  ) +
  geom_boxplot()

# use color argument -- this changes the BORDER of the box (not fill of box)
iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  color = Species)
  ) +
  geom_boxplot()

# change color of box interior -- requires 'fill' argument
iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  fill = Species)
  ) +
  geom_boxplot()



# Fun Plot ----------------------------------------------------------------

# make a violin plot of petal length by species
violin <- iris %>% 
  ggplot(aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_violin()
# print plot
violin

# generate 2d density plot of petal length & sepal length
density_plot_2d <- iris %>% 
  ggplot(aes(x = Petal.Length, y = Sepal.Length)) +
  geom_density_2d()
# print plot
density_plot_2d

# generate 1d density plot of petal length, fill by species
density_plot_1d <- iris %>% 
  ggplot(aes(x = Petal.Length, fill = Species)) +
  geom_density()
# print plot
density_plot_1d





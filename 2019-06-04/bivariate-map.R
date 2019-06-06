
########################################################################
## Project: Tidy Tuesday 2019-06-04
## Script purpose: Create bivariate choropleth map showing relationship
## between number of reviews and average rating of Ramen
##
## Date: 2019-06-06
## Author: csmontt
########################################################################

# This visualization is a simpler version/copy of what Timo Grossenbacher did in
# the following link:
# See https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

library(tidyverse) 
library(magrittr) 
library(sf) 
library(viridis) 
library(cowplot) 
library(tmap)


# Get the Simple Feature collection of the World from tmap package
data("World")

# Convert names which are currently a factor to a character type
World$name <- as.character(World$name)

# Get the data
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")


# Get avergae rating per country
mean_country <- ramen_ratings %>% group_by(country) %>% 
        summarize(mean_rating= mean(stars, na.rm = TRUE),
                  n_obs = n())

# identify countries names that do not match with World sf data
mean_country$country[!(mean_country$country %in% World$name)]

# Change names to the ones that appear in World sf. Eventhough Sarawak is just 
# part of Malaysia I just recode as Malaysia
mean_country$country[mean_country$country == "UK"] <- "United Kingdom"
mean_country$country[mean_country$country == "Phlippines"] <- "Philippines"
mean_country$country[mean_country$country == "USA"] <- "United States"
mean_country$country[mean_country$country == "Sarawak"] <- "Malaysia"
World$name[World$name == "Korea" & World$sovereignt == "South Korea"] <- "South Korea" 

# join mean ratings by country with world data
ramen_world <- left_join(World, mean_country, by = c("name" = "country"))

# Filter data by continent
select_continents <- c("Asia", "Oceania")

ramen_world <- ramen_world %>% filter(continent %in% select_continents,
                                      n_obs > 0)

# If there was no rating I just 
#ramen_world$mean_rating[is.na(ramen_world$mean_rating)] <- 0

# Create a custom them_map
theme_map <- function(...) {
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "#22211d"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    ...
  )
}

# Reproject sf
ramen_world <- st_transform(ramen_world, 3832)


# -------------------------------------------------------------------------------
# create 3 buckets for number of reviews
quantiles_reviews <- ramen_world %>%
  pull(n_obs) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for mean rating
quantiles_mean <- ramen_world %>%
  pull(mean_rating) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for number of reviews and blue for mean rating

bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high number of reviews, high average rating
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low number of reviews, high average rating
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium number of reviews, medium average rating
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high number of reviews, low average rating
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low number of reviews, low average rating
) %>%
  gather("group", "fill")

# -------------------------------------------------------------------------------
# cut into groups defined above and join fill
ramen_world %<>%
  mutate(
    gini_quantiles = cut(
      n_obs,
      breaks = quantiles_reviews,
      include.lowest = TRUE
    ),
    mean_quantiles = cut(
      mean_rating,
      breaks = quantiles_mean,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(gini_quantiles), "-",
      as.numeric(mean_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his number of reviews
  # and average rating
  left_join(bivariate_color_scale, by = "group")

#----------------------------------------------------------------------------------
# map data

map <- ggplot(
  # use the same dataset as before
  data = ramen_world
  ) +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # color countries according to their number of reviews / rating combination
    geom_sf(
    mapping = aes(
      fill = fill
      ),
    color = "white",
    size = 0.1
  ) +
  # as the sf object ramen_world has a column with name "fill" that
  # contains the literal color as hex code for each country, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # add titles
  labs(x = NULL,
         y = NULL,
         title = "Ramen ratings and reviews in Asia and Oceania",
         subtitle = paste0("Average ramen ratings and number of observations per country"
                           )) +
  # add the theme
  theme_map() 


# Create legend for bivaiate color scale -------------------------------------------
bivariate_color_scale %<>%
  separate(group, into = c("gini", "mean"), sep = " - ") %>%
  mutate(gini = as.integer(gini),
         mean = as.integer(mean))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = gini,
      y = mean,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher number of reviews ⟶️",
       y = "Higher average rating ⟶️") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()

# Combine both ---------------------------------------------------------------------
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, 0.075, 0.2, 0.2)


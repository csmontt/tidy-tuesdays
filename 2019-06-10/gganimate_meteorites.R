########################################################################
## Project: Tidy Tuesday 2019-10-04
## Script purpose: Use gganimate package to create an animate vis
## of meteorite collisions over time
##
## Date: 2019-06-10
## Author: csmontt
########################################################################

library(ggplot2)
library(ggthemes)
library(gganimate)


# Get base map
world <- ggplot() +
  borders("world", colour = "#353535", fill = "#353535") +
  theme_map()

# Read data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteorites <- meteorites[!is.na(meteorites$lat) & !is.na(meteorites$year), ]

# Took the log of mass and divided it by 5, so when using this new 
# variable to size the geom_points in ggplot they didn't get too big.
meteorites <- meteorites %>% mutate(log_mass = log(mass)/5) %>%
        filter(year >= 1800 & year <= 20013)


# Specifiy vis
met_map <- world + geom_point(data = meteorites, 
               x = meteorites$long,
               y = meteorites$lat,
               color = "#ffa500",
               alpha = 0.7,
               size = meteorites$log_mass) + 
           transition_states(meteorites$year, 
                          transition_length = 1, 
                          state_length = 1) +
           shadow_mark(past=TRUE) +
           theme(plot.title = element_text(color = "white", 
                                           size = 20, 
                                           face = "bold",
                                           vjust = -10),
                 panel.background = element_rect(fill = "#35535F")) +
           labs(title = "{closest_state}")

options(gganimate.dev_args = list(width = 1000, height = 600))
ani_met <- animate(met_map, nframes = 300, fps=10, detail = 1)

# Save GIF
anim_save("./figures/2019-06-10.gif")

# To do:
# Should have specified more frames as 300 were not enough to show all 
# years with data.
# Add a lbel for the mass of the meteorites, maybe create categories instead
# of using a continous variable.
# add mapping by color to denote type of meteorite.


library(ggplot2)
library(ggthemes)
library(gganimate)

world <- ggplot() +
  borders("world", colour = "#353535", fill = "#353535") +
  theme_map()

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteorites <- meteorites[!is.na(meteorites$lat) & !is.na(meteorites$year), ]

meteorites <- meteorites %>% mutate(log_mass = log(mass)/5) %>%
        filter(year >= 1800 & year <= 20013)



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

anim_save("./figures/2019-06-10.gif")


########################################################################
## Project: tidytuesdays
## Script purpose: a simple annotated boxplot
##
##
## Date: 02-07-2019
## Author: csmontt
########################################################################
library(devtools)
library(extrafont)
library(here)
loadfonts(device = "win")
library(tidyverse)

source(here("2019-07-02", "custom_theme.R"))

# load data --------------------------------------------------------------------------                     
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# delete duplicates
media_franchises <- media_franchises[!duplicated(media_franchises), ]

# get most succesful media franchise by cateory
max_revenue <-  media_franchises %>% group_by(revenue_category) %>% 
        slice(which.max(revenue)) %>% tidyr::unnest()

# create plot
ggplot(media_franchises, 
              aes(x = factor(revenue_category), y = revenue, fill = revenue_category)) + 
        geom_boxplot() + custom_theme() +
        geom_text(data = max_revenue, 
              aes(x = factor(revenue_category), y = revenue, label = franchise, 
                  family = "Century Gothic"), 
              nudge_x = 0, nudge_y = 2, size = 2) +
        labs(title="Most succesful media franchises",
             subtitle = "Revenue in billions by media category",
             caption = "Source: Wikipedia | Vis: @csmontt") +
        guides(fill=guide_legend(nrow=1))

# save plot
ggsave(here("figures", "media_franchise_boxplot.png"), width = 11, height = 6)





                
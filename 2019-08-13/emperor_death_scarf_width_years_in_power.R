library(tidyverse)
library(lubridate)
library(here)
library(extrafont)

loadfonts(device="win")
font <- "Consolas"


source(here("2019-08-13", "custom_theme.R"))

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors$reign_start[1] <- lubridate::ymd("0026/01/16") - lubridate::years(52)
emperors$years_rule <- as.numeric(emperors$reign_end - emperors$reign_start)
emperors$nothing <- "nothing"
emperors2 <- emperors %>% filter(cause != "Unknown")

#Normalized Data
#emperors2$years_norm = (emperors2$years_rule-min(emperors2$years_rule))/(max(emperors2$years_rule)-min(emperors2$years_rule))


p <- ggplot(emperors2, aes(x = 1:nrow(emperors2), y=nothing,  fill=cause)) +
  geom_tile(color="#f5f5f2", height=0.3, width = emperors2$years_rule/365) +
        custom_theme() + guides(fill = guide_legend(nrow = 1)) +
        scale_fill_brewer(palette="Set3") +
        labs(title = "How Roman Emperors Died",
        subtitle = "Each tile corresponds to a Roman Emperor. They are displayed in chronological order from left to right.",
        caption = "Data: Wikipedia via @geokaramanis | Vis: @Cristobal_Montt") 
p


p2 <- ggplot(emperors2, aes(x = 1:nrow(emperors2), y=nothing,  fill=cause)) +
  geom_tile(color="#f5f5f2", height=0.3, width = emperors2$years_rule) +
        custom_theme() + guides(fill = guide_legend(nrow = 1)) +
        scale_fill_brewer(palette="Set3") +
        labs(title = "How Roman Emperors Died",
        subtitle = "Each tile corresponds to a Roman Emperor. They are displayed in chronological order from left to right.",
        caption = "Data: Wikipedia via @geokaramanis | Vis: @Cristobal_Montt") 
p2



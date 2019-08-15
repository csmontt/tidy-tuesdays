library(tidyverse)
library(lubridate)
library(here)
library(extrafont)

loadfonts(device="win")
font <- "Consolas"


source(here("2019-08-13", "custom_theme2.R"))

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors$reign_start[1] <- lubridate::ymd("0026/01/16") - lubridate::years(52)
emperors$years_rule <- as.numeric(emperors$reign_end - emperors$reign_start)


emperors$gp <- seq(from = nrow(emperors), to = 1)
p3 <- ggplot(emperors, aes(x="thing", y=years_rule, fill=cause, group=factor(gp))) + 
       geom_bar(stat="identity", colour="#f5f5f2", width = 0.3, size = 0.001)  +
       scale_fill_brewer(palette="Set3") +
       guides(fill = guide_legend(nrow = 1)) +
       labs(title = "\n\n\n\n\n\n\n\n\nHow Roman Emperors Died",
       subtitle = "one tile one emperor",
       caption = "Data: Wikipedia via @geokaramanis | Vis: @Cristobal_Montt") +
       scale_y_continuous(breaks = c(0, 50000, 150000, 200000)) +
       coord_flip() +
       custom_theme2() +
       annotate("text", x = 0.831, y = 100000, label = "Days since the beginning of the Roman Empire", 
             color = "black", size = 1.8, family = font)
p3

ggsave(here("figures", "2019-08-13c.png"), plot = p3, width = 10, height = 6)  

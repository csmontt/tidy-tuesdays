
library(tidyverse)
library(lubridate)
library(here)

loadfonts(device="win")
font <- "Consolas"


source(here("2019-08-13", "custom_theme.R"))

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors$reign_start[1] <- lubridate::ymd("0026/01/16") - lubridate::years(52)
emperors$years_rule <- emperors$reign_end - emperors$reign_start
emperors$nothing <- "nothing"
emperors <- emperors %>% filter(cause != "Unknown")


p <- ggplot(emperors, aes(x = 1:nrow(emperors), y=nothing,  fill=cause)) +
  geom_tile(color="#f5f5f2", height=0.3) +
        custom_theme() + guides(fill = guide_legend(nrow = 1)) +
        scale_fill_brewer(palette="Set3") +
        labs(title = "How Roman Emperors Died",
        subtitle = "Each tile corresponds to a Roman Emperor. They are displayed in chronological order from left to right.",
        caption = "Data: Wikipedia via @geokaramanis | Vis: @Cristobal_Montt") 

p + annotate("rect", xmin = 0.39, xmax = 0.44, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 5.5, xmax = 5.55, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 11.5, xmax = 11.55, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 18.48, xmax = 18.53, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 26.4, xmax = 26.45, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 46.5, xmax = 46.55, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 60.4, xmax = 60.45, ymin = 0.852, ymax = 1.2, color = "black") +
    annotate("rect", xmin = 64.4, xmax = 64.45, ymin = 0.852, ymax = 1.2, color = "black") + 
    annotate("text", x = 2.9, y = 1.19, label = "Julio-Claudian", 
             color = "black", size = 2.23, family = font) +
    annotate("text", x = 8.4, y = 1.19, label = "Flavian", 
             color = "black", size = 2.6, family = font) +
    annotate("text", x = 15, y = 1.19, label = "Nerva-Antonine", 
             color = "black", size = 2.6, family = font) +
    annotate("text", x = 22.3, y = 1.19, label = "Severan", 
             color = "black", size = 2.6, family = font) +
    annotate("text", x = 36, y = 1.19, label = "Gordian", 
             color = "black", size = 2.6, family = font) + 
    annotate("text", x = 53.5, y = 1.19, label = "Constantinian", 
             color = "black", size = 2.6, family = font) +
    annotate("text", x = 62.4, y = 1.19, label = "Valentinian", 
             color = "black", size = 2.23, family = font)
   


# other plots ----------------------------------------------------------------------------
#new_dest <- paste0(emperors$name, " (", emperors$reign_start, ")")
#emperors$names_ordered <- reorder(emperors$name, emperors$reign_start)

#ggplot(emperors, aes(x = reign_start, y=names_ordered,  fill=cause)) +
#  geom_tile(color="#f5f5f2", width=emperors$years_rule, height=1) +
#        custom_theme()

#ggplot(emperors, aes(x = reign_start, y=nothing,  fill=cause)) +
#  geom_tile(color="#f5f5f2", width=emperors$years_rule*1.3, height=1) +
#        custom_theme() 

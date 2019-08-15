
library(tidyverse)
library(lubridate)
library(here)
library(extrafont)

loadfonts(device="win")
font <- "Consolas"


source(here("2019-08-13", "custom_theme.R"))

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors$reign_start[1] <- lubridate::ymd("0026/01/16") - lubridate::years(52)
emperors$years_rule <- emperors$reign_end - emperors$reign_start
emperors$nothing <- "nothing"
emperors2 <- emperors %>% filter(cause != "Unknown")


p <- ggplot(emperors2, aes(x = 1:nrow(emperors2), y=nothing,
                           fill=cause)) +
  geom_tile(color="#f5f5f2", height=0.35) +
        custom_theme() + guides(fill = guide_legend(nrow = 1)) +
        scale_fill_brewer(palette="Set3") +
        scale_x_continuous(expand = c(0,0)) +
        labs(title = "\n\n\n\n\n\n\n\n\n\n\nHow Roman Emperors Died",
        subtitle = "                                                                       one tile one emperor",
        caption = "Data: Wikipedia via @geokaramanis | Vis: @Cristobal_Montt") 
p  

p2 <- p + geom_segment(aes(x = 0.5, y = 0.81, xend = 0.5, yend = 1.2),linetype = "dotted") + 
#annotate("rect", linetype = "dotted", xmin = 0.47, xmax = 0.48, ymin = 0.81, ymax = 1.2, color = "black") +
    #annotate("rect", linetype = "dotted", xmin = 5.5, xmax = 5.51, ymin = 0.81, ymax = 1.2, color = "black") +
    geom_segment(aes(x = 5.52, y = 0.81, xend = 5.52, yend = 1.2),linetype = "dotted") +
        geom_segment(aes(x = 11.51, y = 0.81, xend = 11.51, yend = 1.2),linetype = "dotted") +
        geom_segment(aes(x = 18.50, y = 0.81, xend = 18.50, yend = 1.2),linetype = "dotted") +
        geom_segment(aes(x = 26.51, y = 0.81, xend = 26.51, yend = 1.2),linetype = "dotted") +
        geom_segment(aes(x = 46.5, y = 0.81, xend = 46.5, yend = 1.2),linetype = "dotted") +
        geom_segment(aes(x = 60.5, y = 0.81, xend = 60.5, yend = 1.2),linetype = "dotted") +
        geom_segment(aes(x = 64.52, y = 0.81, xend = 64.52, yend = 1.2),linetype = "dotted") +
    # annotate("rect", linetype = "dotted", xmin = 11.5, xmax = 11.51, ymin = 0.81, ymax = 1.2, color = "black") +
    # annotate("rect", linetype = "dotted", xmin = 18.48, xmax = 18.49, ymin = 0.81, ymax = 1.2, color = "black") +
    # annotate("rect", linetype = "dotted", xmin = 26.49, xmax = 26.5, ymin = 0.81, ymax = 1.2, color = "black") +
    # annotate("rect", linetype = "dotted", xmin = 46.5, xmax = 46.51, ymin = 0.81, ymax = 1.2, color = "black") +
    # annotate("rect", linetype = "dotted", xmin = 60.48, xmax = 60.49, ymin = 0.81, ymax = 1.2, color = "black") +
    # annotate("rect", linetype = "dotted", xmin = 64.49, xmax = 64.5, ymin = 0.81, ymax = 1.2, color = "black") + 
    annotate("text", x = 2.98, y = 1.23, label = "Julio-Claudian", 
             color = "black", size = 2.23, family = font) +
    annotate("text", x = 8.4, y = 1.23, label = "Flavian", 
             color = "black", size = 2.23, family = font) +
    annotate("text", x = 15, y = 1.23, label = "Nerva-Antonine", 
             color = "black", size = 2.23, family = font) +
    annotate("text", x = 22.3, y = 1.23, label = "Severan", 
             color = "black", size = 2.23, family = font) +
    annotate("text", x = 36, y = 1.23, label = "Gordian", 
             color = "black", size = 2.23, family = font) + 
    annotate("text", x = 53.5, y = 1.23, label = "Constantinian", 
             color = "black", size = 2.23, family = font) +
    annotate("text", x = 62.5, y = 1.23, label = "Valentinian", 
             color = "black", size = 2.23, family = font)
   
ggsave(here("figures", "2019-08-13.png"), plot = p, width = 10, height = 6)  
ggsave(here("figures", "2019-08-13b.png"), plot = p2, width = 10, height = 6)  

# other plots ----------------------------------------------------------------------------
# need to change the theme so the axis are visible
new_dest <- paste0(emperors$name, " (", emperors$reign_start, ")")
emperors$names_ordered <- reorder(emperors$name, emperors$reign_start)

ggplot(emperors, aes(x = reign_start, y=names_ordered,  fill=cause)) +
  geom_tile(color="#f5f5f2", width=emperors$years_rule, height=1) +
        custom_theme()

ggplot(emperors, aes(x = reign_start, y=nothing,  fill=cause)) +
  geom_tile(color="#f5f5f2", width=emperors$years_rule*1.3, height=1) +
        custom_theme()

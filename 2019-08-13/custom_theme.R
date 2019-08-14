custom_theme <- function () 
{
    font <- "Consolas"
    ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                         size = 10, face = "bold", color = "#222222", hjust = 0.5), 
                         plot.subtitle = ggplot2::element_text(family = font, 
                         size = 8, 
                         margin = ggplot2::margin(1, 0, -30, 0)), 
                   legend.position = c(0.50, 0.15), 
                   #legend.position = "none",
                   legend.text.align = 0, 
                   legend.title = ggplot2::element_blank(), 
                   legend.key = ggplot2::element_blank(), 
                   legend.key.size = unit(0.65,"line"),
                   legend.text = ggplot2::element_text(family = font, size = 6, 
                   color = "black"), 
                   legend.direction = "horizontal",
                   axis.title = ggplot2::element_blank(), 
                   axis.text = ggplot2::element_text(family = font, size = 8, 
                   color = "#222222"), 
                   axis.text.x = ggplot2::element_blank(), # , angle = 20
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), 
                   axis.line = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), 
                   # facet labels
                   #strip.background = element_rect(color="transparent", 
                   #                                fill="transparent"),
                   #strip.text.x = element_text(size = 12, color = "black",
                   #                            family = font,
                   #                            face = "bold"), #, face = "bold.italic"
                   #horizontal line color
                   plot.caption=element_text(hjust=1,size=5, family = font),
        panel.grid.major.y = ggplot2::element_blank(), 
        panel.grid.major.x = ggplot2::element_blank(), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        strip.text = ggplot2::element_text(size = 10, hjust = 0)) 
}
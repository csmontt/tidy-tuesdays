custom_theme <- function () 
{
    font <- "Century Gothic"
    ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                         size = 18, face = "bold", color = "#222222"), 
                         plot.subtitle = ggplot2::element_text(family = font, 
                         size = 12, 
                         margin = ggplot2::margin(1, 0, 5, 0)), 
                   #legend.position = "top", 
                   legend.position = "none",
                   legend.text.align = 0, 
                   legend.title = ggplot2::element_blank(), 
                   legend.key = ggplot2::element_blank(), 
                   #legend.text = ggplot2::element_text(family = font, size = 8, 
                   #color = "#222222"), 
                   legend.direction = "horizontal",
                   axis.title = ggplot2::element_blank(), 
                   axis.text = ggplot2::element_text(family = font, size = 6, 
                   color = "#222222"), 
                   axis.text.x = element_text(face = "bold", vjust = 10), # , angle = 20
                   axis.ticks = ggplot2::element_blank(), 
                   axis.line = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), 
                   #horizontal line color
                   plot.caption=element_text(hjust=1,size=5),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
        panel.grid.major.x = ggplot2::element_blank(), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        strip.text = ggplot2::element_text(size = 10, hjust = 0))
}


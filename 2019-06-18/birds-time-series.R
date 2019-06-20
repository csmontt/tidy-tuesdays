########################################################################
## Project: Tidytuesday 2019-06-18
## Script purpose: Create an animated time series of bird counts per hour
## over the last 60 years
##
## Date: 2019-06-18
## Author: csmontt
########################################################################
options(scipen = 999)

library(tidyverse)
library(plotly)
library(animation)
library(RColorBrewer)
library(here)

source(here("2019-06-18", "theme_custom.R"))

# idea from 
# https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")


# consider years where there is no missing data
nas_year <-  bird_counts %>% group_by(year) %>% 
        summarize(total_nas = sum(is.na(how_many_counted_by_hour)))

# get the latest interval of years without missing values 
# It gives back the row index of the last year with missing data
nas_year$missing <- nas_year$total_nas > 0
last_ind_missing = NA
for(i in 1:nrow(nas_year)){
        if (nas_year$missing[i] == TRUE){
                last_ind_missing <- i
        } 
}

nas_year <- nas_year[last_ind_missing + 1:nrow(nas_year), ]

# Im keeping only the interval of years without missing data (every year since
# 1950)
bird_counts <- bird_counts %>% filter(bird_counts$year %in% nas_year$year)

# keep the three most observed species over time, otherwise plot gets too 
# clutter
most_observed <- bird_counts %>% group_by(species) %>% 
        summarize(total_obs  = sum(how_many_counted)) %>%
        arrange(desc(total_obs)) %>% head(3)

most_obs_species <- most_observed$species


bird_filtered <- bird_counts %>% 
        filter(species %in% most_obs_species) %>%
        select(year, species, how_many_counted_by_hour)

# convert species to factor
bird_filtered$species<-as.factor(bird_filtered$species)


# animation --------------------------------------------------------------------
# set some of the options 
ani.options(interval = 0.2, 
            nmax = 100, ani.width = 800)

## The good animation as a simple GIF
saveGIF({
  end_year = 2017 #last year of the plot
  num_years = length(unique(bird_filtered$year)) #number of years in the animation
  #create a loop that does the subsetting
  for(i in 1:num_years){
    bird_subset <- bird_filtered %>% filter(year <= end_year-(num_years-i))
    #write the plot with a subset
    p<-ggplot(bird_subset,aes(x=year,y=how_many_counted_by_hour,
                                group=species,colour=species)) +
      geom_line(size = 1.5) +
      scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2017)) +
      ylim(0,440)+
      xlab('') +
      ylab('') +
      scale_colour_brewer(palette="Dark2") +
      theme_custom() +
      labs(title="Birds count per hour", caption="Data: www.birdscanada.org | Vis: @cristobal_montt") +
      guides(fill=guide_legend(title="Species"))
      print(p)
  }#close the for loop
  
}, movie.name = here("figures", "2019-06-18.gif")) #close the animation builder






########################################################################
## Project: #TidyTuesday DBSCAN meteorites + 'population lines'
## Script purpose: Cluster meteorites found on earth and depict the areas
## with a high density of impacts
##
## Date: 2019-06-20
## Author: csmontt
########################################################################

# Idea from https://www.whackdata.com/2014/08/04/line-graphs-parallel-processing-r/

# 1) First, get a map of the world
# https://stackoverflow.com/questions/20146809/how-can-i-plot-a-continents-map-with-r
library(tidyverse)
library(dbscan)
library(rworldmap)
library(rgeos)
library(maptools)
library(cleangeo)  ## For clgeo_Clean()
library(graphics)
library(tcltk)
library(pracma)
library(data.table)
library(doParallel)
library(here)

sPDF <- getMap()
sPDF <- clgeo_Clean(sPDF)  ## Needed to fix up some non-closed polygons 
cont <-
    sapply(levels(sPDF$continent),
           FUN = function(i) {
               ## Merge polygons within a continent
               poly <- gUnionCascaded(subset(sPDF, continent==i))
               ## Give each polygon a unique ID
               poly <- spChFIDs(poly, i)
               ## Make SPDF from SpatialPolygons object
               SpatialPolygonsDataFrame(poly,
                                        data.frame(continent=i, row.names=i))
           },
           USE.NAMES=TRUE)

## Bind the 6 continent-level SPDFs into a single SPDF
cont <- Reduce(spRbind, cont)


#-------------------------------------------------------------------------------
# 2) Extract coords of continent polygons. I do this so I can plot the border
# of the continents.

extractCoords <- function(spdf){
    results <- data.frame()
    for(i in 1:length(spdf@polygons)){
            pols <- length(spdf@polygons[[i]]@Polygons)
            for(j in 1:pols){
                  results <- rbind(results, spdf@polygons[[i]]@Polygons[[j]]@coords)  
            }
    }
    results
}



coords_world <- extractCoords(cont)

names(coords_world) <- c("x", "y")

# I need to asign a value above 0 so the borders contrast with the rest of the line
coords_world$frequency <- 5

all.data <- as.data.table(coords_world)


#--------------------------------------------------------------------------------------------
# Read meteorite data 
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
met_coords <- meteorites %>% dplyr::select(long, lat) %>% na.omit

#mets_per_coord <- meteorites %>% group_by(lat, long) %>% summarise(tot = n())


# Run dbscan ------------------------------------------------------------------------------
EPS <- 0.2 # the smaller the number the smaller the neighborhood
min_points = 100 # minimum number of points in a neighborhood / minimum number of 
                 # meteorite impacts
clusters <- dbscan(dplyr::select(met_coords, lat, long), eps = EPS, minPts = min_points)
met_coords$cluster <- clusters$cluster
length(unique(met_coords$cluster <- clusters$cluster))

groups  <- met_coords %>% filter(cluster != 0)
noise  <- met_coords %>% filter(cluster == 0)

# calculate center of clusters ------------------------------------------------------------
met_coords <- met_coords %>% filter(cluster != 0)

mean_clusters <- met_coords %>% group_by(cluster) %>% summarize(mean_lon = mean(long),
                                                                 mean_lat = mean(lat),
                                                                 total_obs = n())

mean_clusters <- mean_clusters %>% dplyr::select(mean_lon, mean_lat, total_obs)

names(mean_clusters) <- c("x", "y", "frequency")


all.data <- rbind(all.data, mean_clusters)


# Create population lines plot ------------------------------------------------------------
# This is just copy paste from https://www.whackdata.com/2014/08/04/line-graphs-parallel-processing-r/
startEnd <- function(lats, lngs) {
  # Find the "upper left" (NW) and "bottom right" (SE) coordinates 
  # of a set of data.
  #
  # Args:
  #  lats: A list of latitude coordinates
  #  lngs: A list of longitude coordinates
  #
  # Returns: 
  #   A list of values corresponding to the northwest-most and 
  # southeast-most coordinates
  
  # Convert to real number and remove NA values
  lats <- na.omit(as.numeric(lats))
  lngs <- na.omit(as.numeric(lngs))
  
  topLat <- max(lats)
  topLng <- min(lngs)
  botLat <- min(lats)
  botLng <- max(lngs)
  
  return(c(topLat, topLng, botLat, botLng))
}

startEndVals <- startEnd(all.data$y, all.data$x)
remove(startEnd)

startLat <- startEndVals[1]
endLat <- startEndVals[3]
startLng <- startEndVals[2]
endLng <- startEndVals[4]
remove(startEndVals)

interval.v.num = 200.0
interval.h.num = 800.0
interval.v <- (startLat - endLat) / interval.v.num
interval.h <- (endLng - startLng) / interval.h.num
remove(num_intervals)

lat.list <- seq(startLat, endLat + interval.v, -1*interval.v)

# testLng <- -66.66152983 # Fredericton
# testLat <- 45.96538183 # Fredericton

# Prepare the data to be sent in
# If you have a value you want to sum, use this
data <- all.data[,list(x, y, frequency)]

# If you want to perform a count, use this
# data <- all.data[,list(x, y)]
# data[,Value:=1]

sumInsideSquare <- function(pointLat, pointLng, data) {
  # Sum all the values that fall within a square on a map given a point,
  # an interval of the map, and data that contains lat, lng and the values
  # of interest
  
  setnames(data, c("lng", "lat", "value"))
  
  # Get data inside lat/lon boundaries
  lng.interval <- c(pointLng, pointLng + interval.h)
  lat.interval <- c(pointLat - interval.v, pointLat)
  data <- data[lng %between% lng.interval][lat %between% lat.interval]
  
  return(sum(data$value))
}

# Debugging
# squareSumTemp <- sumInsideSquare(testLat, testLng, interval, data)

# Given a start longitude and an end longitude, calculate an array of values
# corresponding to the sums for that latitude

calcSumLat <- function(startLng, endLng, lat, data) {
  row <- c()
  lng <- startLng
  while (lng < endLng) {
    row <- c(row, sumInsideSquare(lat, lng, data))
    lng <- lng + interval.h
  }
  
  return(row)
}

# Debugging
# rowTemp <- calcSumLat(startLng, endLng, testLat, interval, data)
# write.csv(rowTemp, file = "Temp.csv", row.names = FALSE)

# Set up parallel computing with the number of cores you have
cl <- makeCluster(detectCores(), outfile = "./Progress.txt")
registerDoParallel(cl)

all.sums <- foreach(lat=lat.list, .packages=c("data.table")) %dopar% {
  
  lat.data <- calcSumLat(startLng, endLng, lat, data)
  
  # Progress indicator that works on Mac/Windows
  print((startLat - lat)/(startLat - endLat)*100) # Prints to Progress.txt
  
  lat.data
  
}

stopCluster(cl = cl)

# Convert to data frame
all.sums.table <- as.data.table(all.sums)



output.file <- here("2019-06-10", "meteorites.csv")

write.csv(all.sums.table, file = output.file, row.names = FALSE)


# Load the data generated by 01GenerateData.R   # WorldPopulation.csv
plot.data <- read.csv(output.file, header=TRUE, stringsAsFactors=FALSE)


# Add padding above/below where there was data

# On top
top.padding <- 1:1
for (i in top.padding) {
  plot.data <- cbind(0, plot.data)
}
# On bottom
bottom.padding <- 1:1
for (i in bottom.padding) {
  plot.data <- cbind(plot.data, 0)
}

# On left
zero.row <- vector(mode="integer", length=dim(plot.data)[1])

left.padding <- 1:10
for (i in left.padding) {
  plot.data <- rbind(zero.row, plot.data)
}

# On right
right.padding <- 1:10
for (i in left.padding) {
  plot.data <- rbind(plot.data, zero.row)
}


max <- max(plot.data) # Max value in the data, used for scaling
plottingHeight <- 1000 # Arbitrary number that provides the graph's height
scaleFactor <- 300 # Discovered through trial and error to keep the graph in the boundaries
gap <- plottingHeight / length(plot.data) # Space between lines

plot.width = 25
plot.height = 15
svg(filename = "./figures/meteorites.svg", pointsize=12, width=plot.width, height=plot.height)

# Create a blank plot
yVals <- as.vector(plot.data[[1]] / max * scaleFactor)
plot(0, 0, xlim=c(0, length(yVals)), ylim=c(0,1100), type="n", las=1, xlab=NA, ylab=NA, bty="n", axes=FALSE)

plotting.threshold <- 0.1

plot.length = length(plot.data)

# Plot each line
for (i in 1:plot.length) {
  # Grabs a row of data
  yVals <- as.vector(plot.data[[i]] / max * scaleFactor)
  xVals <- c(0:(length(yVals) - 1))
  yVals.smooth =  savgol(yVals, 3, forder=4)
  
  polygon(xVals, yVals.smooth + plottingHeight, border = NA, col = "#ffffff")
  lines(xVals, yVals.smooth + plottingHeight, col="#8C8C8C", lwd=0.5)
  
  #Plot the peaks with a darker line.
  j <- 2 # Skip padding
  while (j <= (length(yVals.smooth) - 2)) {

    if ((yVals.smooth[j]) > plotting.threshold | (yVals.smooth[j+1]) > plotting.threshold) {
      segments(xVals[j], yVals.smooth[j] + plottingHeight, xVals[j+1], yVals.smooth[j+1] + plottingHeight, col="#000000", lwd=1.5)
    } else { } # Do nothing

    j <- j + 1

  }
  plottingHeight <- plottingHeight - gap

}

dev.off()

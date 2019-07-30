########################################################################
## Project: Women's World Cup 2019 Network Visualization
## Script purpose: To explore how countries and teams are connected
## Date: 15-07-2019
## Author: csmontt
########################################################################

options(stringsAsFactors = FALSE)

library(tidyverse)
library(here)
library(rvest)
library(R.utils)
library(tidygraph) # tidy graph analysis
library(ggraph)    # for plotting


# Scrape data ------------------------------------------------------------------------

url <-
  "https://es.wikipedia.org/wiki/Anexo:Equipos_participantes_en_la_Copa_Mundial_Femenina_de_F%C3%BAtbol_de_2019"

html_content <- url %>%
  read_html()

# get clubs ----
clubs <- html_content %>%
  html_nodes("td:nth-child(6) , .jquery-tablesorter .flagicon+ a") %>%
  html_text() %>% gsub("^\\s+|\\s+$", "", .)

clubs <- gsub("Sevilla FC", "Sevilla", clubs)

# Get country of clubs ----
flags <- html_content %>%
  html_nodes("a img") %>% html_attr("alt")

flags <- flags[c(-(length(flags) - 1),-length(flags))]

flags <- flags[-grep("Capitán", flags)]

inds <-
  grep("equipo", clubs)[1:3] #just the first three don't have flags

country_club <- insert(flags, inds, values = "?")

country_club <-
  gsub("República Popular China", "RP China", country_club)



# get countries ----
nationality <- html_content %>%
  html_nodes("h2+ h3 , .plainrowheaders+ h3") %>%
  html_text() %>% gsub("^\\s+|\\s+$", "", .) %>% gsub("\\[editar\\]", "", .)

nationality <-
  gsub("República Popular China", "RP China", nationality)
nationality <- rep(nationality, each = 23)



# combine all ----

df <- as.data.frame(cbind(nationality, clubs, country_club))

df <- df %>% group_by(nationality, clubs) %>% mutate(width = n() * 5,
                                                     n_players = n()) # *5 so in visnetwork
# the width is noticeable
# in ggraph changes the legend
# for visnetwork have
# to call it width

df <- df[df$country_club != "?", ]


# Vis --------------------------------------------------------------------------------
cuartos <-
  c(
    "Noruega",
    "Inglaterra",
    "Francia",
    "Estados Unidos",
    "Italia",
    "Países Bajos",
    "Suecia",
    "Alemania"
  )

df2 <- df[df$nationality %in% cuartos,]

df2 <- df

country_origin_club <- df2[, c("clubs", "country_club")]

# I wanted to connect teams of countries that appear on other countries but who were
# not connected to their country of origin, for that, I just reverse nationality and
# club, and established the weight as 0. In that way, teams origintated in a country
# are closer to that country, rather than isolated.

df_con <- df2[, c(3, 2, 1)]
names(df_con) <- c("nationality", "clubs", "country_club")
df_con$n_players <- 0

df2 <- rbind(df2, df_con)

# there is some annoying character in netherlands name.
df2$nationality <-
  gsub("P...es..ajos", "Países Bajos", df2$nationality)
df2$country_club <-
  gsub("P...es..ajos", "Países Bajos", df2$country_club)


# Create graph -----
df_graph <- as_tbl_graph(df2, directed = FALSE) %>%
  mutate(
    centrality_authority = centrality_authority(),
    centrality_betweenness = centrality_betweenness(),
    centrality_closeness = centrality_closeness(),
    centrality_pagerank = centrality_pagerank(),
    centrality_degree = centrality_degree(),
    centrality_eigen = centrality_eigen(),
    group = group_infomap(),
    center = node_is_center(),
    dist_to_center = node_distance_to(node_is_center()),
    keyplayer = node_is_keyplayer(k = 10)
  ) %>%
  activate(edges) %>%
  #filter(!edge_is_multiple()) %>%
  mutate(centrality_e = centrality_edge_betweenness())

#------------------------------------------------------------------------------
# create layout ----
layout <- create_layout(df_graph,
                        layout = "fr")


# Add country of origin to layout, to colour the nodes by country
club_layout <- as.data.frame(layout$name)
names(club_layout) <- "name"

#df3 <- df2[, c("clubs", "country_club")]

club_layout2 <-
  left_join(club_layout, country_origin_club, by = c("name" = "clubs"))

club_layout2$country_club <-
  ifelse(is.na(club_layout2$country_club),
         club_layout2$name,
         club_layout2$country_club)
club_layout2 <- club_layout2[!duplicated(club_layout2),]

layout$group <- club_layout2$country_club

# Create vis ----
# extracting nodes and edges -------------------------------------------

# making color vary according to country of origin of club
# didnt work perfect...chelc
df_graph2 <-
  df_graph %>% activate(nodes) %>% mutate(group = layout$group)

# hay errores sale que psg es de suecia!
#tible_df <- df_graph %>% activate(nodes) %>% mutate(group = layout$group) %>% as_tibble()

library(visNetwork)
df_graph2 %>%
  visIgraph(idToLabel = TRUE) %>% # remove long url labels from underneath nodes
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"))
#degree = list(from = 0, to = 10))) #%>%
#)#visLegend()


# centrality df
nations <- unique(nationality)
nations_df <- layout[layout$name %in% nations, ]

to_english <- c("France", "Nigeria", "Norway", "South Korea", "China PR", "Germany",
                "South Africa", "Spain", "Australia", "Brazil", "Italy", "Jamaica",
                "Argentina", "England", "Japan", "Scotland", "Cameroon", "Canada",
                "Netherlands", "New Zealand", "Chile", "Sweden", "Thailand", 
                "United States")
nations_df$name <- to_english

#do goals_diff_centrality.R

nations_df <- left_join(team_stats, nations_df, by =c("country" = "name"))

ggplot(nations_df, aes(x=centrality_authority, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm) + geom_text(aes(label=country)) 

ggplot(nations_df, aes(x=centrality_betweenness, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm) + geom_text(aes(label=country))  

ggplot(nations_df, aes(x=centrality_closeness, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm) + geom_text(aes(label=country)) 

ggplot(nations_df, aes(x=centrality_pagerank, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm) + geom_text(aes(label=country)) 

ggplot(nations_df, aes(x=centrality_degree, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)  + geom_text(aes(label=country))

ggplot(nations_df, aes(x=centrality_eigen, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)+ geom_text(aes(label=country))

# lets eliminate france, england, spain
del <- c("England", "France", "Spain")
nations_df2 <- nations_df[!(nations_df$country %in% del), ]

ggplot(nations_df2, aes(x=centrality_eigen, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)+ geom_text(aes(label=country))

library(ggstatsplot)

ggstatsplot::ggscatterstats(data = nations_df, x = centrality_degree, y = success)
ggstatsplot::ggscatterstats(data = nations_df, x = centrality_closeness, y = success)
ggstatsplot::ggscatterstats(data = nations_df2, x = centrality_eigen, y = success)
ggplot(nations_df2, aes(x=centrality_eigen, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)+ geom_text(aes(label=country))

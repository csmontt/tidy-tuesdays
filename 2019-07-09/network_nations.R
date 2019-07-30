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


df2 <- df[, c(1,3,4,5)]

# there is some annoying character in netherlands name.
df2$nationality <-
  gsub("P...es..ajos", "Países Bajos", df2$nationality)
df2$country_club <-
  gsub("P...es..ajos", "Países Bajos", df2$country_club)


# Create graph -----
df_graph <- as_tbl_graph(df2, directed = FALSE) %>%
  mutate(
    n_rank_trv = node_rank_traveller(),
    centrality_degree = centrality_degree(),
    group = group_infomap(),
    center = node_is_center(),
    dist_to_center = node_distance_to(node_is_center()),
    keyplayer = node_is_keyplayer(k = 10),
    centrality_auth = centrality_authority()
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

# Create vis ----
# extracting nodes and edges -------------------------------------------

# making color vary according to country of origin of club
# didnt work perfect...chelc
#df_graph2 <-
#  df_graph %>% activate(nodes) %>% mutate(group = layout$group)

# hay errores sale que psg es de suecia!
#tible_df <- df_graph %>% activate(nodes) %>% mutate(group = layout$group) %>% as_tibble()

library(visNetwork)
df_graph%>%
  visIgraph(idToLabel = TRUE) %>% # remove long url labels from underneath nodes
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) 


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

ggplot(nations_df, aes(x=centrality_auth, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)   # Add linear regression line 

ggplot(nations_df, aes(x=dist_to_center, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)  

ggplot(nations_df, aes(x=centrality_degree, y=success)) + geom_point() + # Use hollow circles
    geom_smooth(method=lm)  


library(ggstatsplot)

ggstatsplot::ggscatterstats(data = nations_df, x = centrality_degree, y = success)

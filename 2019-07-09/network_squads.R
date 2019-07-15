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

url <- "https://es.wikipedia.org/wiki/Anexo:Equipos_participantes_en_la_Copa_Mundial_Femenina_de_F%C3%BAtbol_de_2019"

html_content <- url %>% 
  read_html()

# get clubs ----
clubs <- html_content %>%
                html_nodes("td:nth-child(6) , .jquery-tablesorter .flagicon+ a") %>%
                html_text() %>% gsub("^\\s+|\\s+$", "", .)



# Get country of clubs ----
flags <- html_content %>%
                html_nodes("a img") %>% html_attr("alt")

flags <- flags[c(-(length(flags)-1), -length(flags))]

flags <- flags[-grep("Capitán", flags)]

inds <- grep("equipo", clubs)[1:3] #just the first three don't have flags

country_club <- insert(flags, inds, values="?")



# get countries ----
nationality <- html_content %>%
                html_nodes("h2+ h3 , .plainrowheaders+ h3") %>%
                html_text() %>% gsub("^\\s+|\\s+$", "", .) %>% gsub("\\[editar\\]", "", .)


nationality <- rep(nationality, each=23)



# combine all ----

df <- as.data.frame(cbind(nationality, clubs, country_club))

df <- df %>% group_by(nationality, clubs) %>% mutate(n_players = n()) # *5 so in visnetwork
                                                                    # the width is noticeable
                                                                    # in ggraph changes the legend
                                                                    # for visnetwork have
                                                                    # to call it width


# Vis --------------------------------------------------------------------------------
cuartos <- c("Noruega", "Inglaterra", "Francia", "Estados Unidos", "Italia", 
             "Países Bajos", "Suecia", "Alemania")

df2 <- df[df$nationality %in% cuartos, ]


# there is some annoying character in netherlands name.
df2$nationality <- gsub("P...es..ajos", "Países Bajos", df2$nationality)
df2$country_club <- gsub("P...es..ajos", "Países Bajos", df2$country_club)


# Create graph -----
df_graph <- as_tbl_graph(df2, directed = FALSE) %>%
  mutate(n_rank_trv = node_rank_traveller(),
         neighbors = centrality_degree(),
         group = group_infomap(),
         center = node_is_center(),
         dist_to_center = node_distance_to(node_is_center()),
         keyplayer = node_is_keyplayer(k = 10)) %>%
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>%
  mutate(centrality_e = centrality_edge_betweenness())



# create layout ----
layout <- create_layout(df_graph, 
                        layout = "fr")


# Add country of origin to layout, to colour the nodes by country
club_layout <- as.data.frame(layout$name)
names(club_layout) <- "name"
df3 <- df2[, c("clubs", "country_club")]
df3 <- df3[!duplicated(df3), ]
club_layout2 <- left_join(club_layout, df3, by = c("name" = "clubs"))
club_layout2$country_club <- ifelse(is.na(club_layout2$country_club), club_layout2$name, 
                               club_layout2$country_club)
layout$group <- club_layout2$country_club

# Create vis ----
ggraph(layout) + 
    geom_edge_density(aes(fill = n_players)) +
    geom_edge_link(aes(width = n_players), alpha = 0.2) + 
    geom_node_point(aes(color = factor(group)), size = 3) +
    geom_node_text(aes(label = name), size = 2, repel = TRUE) +
    theme_graph() +
    labs(title = "Women's World Cup 2019: Where do players of each country play?",
         subtitle = "The connectiveness of quarter round finalists",
         caption = "Source: Wikipedia | Vis: @csmontt") +
    scale_colour_discrete(name  ="Country") #,
                          #labels = c("Germany", "Spain", "United States", 
                          #           "France", "England", "Italy", "Norway",
                          #           "Netherlands", "Sweden"))
                                   

ggsave(here("figures", "network_football_women.png"), width = 11, height = 6)


# option, use visNetwork
library(visNetwork)
df_graph %>% as.igraph() %>%
  visIgraph(idToLabel = TRUE) #%>% # remove long url labels from underneath nodes
  #visOptions(highlightNearest = TRUE) %>%
  #visLegend()
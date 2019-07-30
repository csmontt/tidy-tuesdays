library(tidyverse)
library(janitor)
library(lubridate)
library(ggforce)
library(here)
library(extrafont)


loadfonts(device="win")
# clean dataset from lizawood's github -----------------------------
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
  janitor::clean_names() 

# clean up some of the factors and playtime data
clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))) %>% 
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>% 
  rename(publisher = publisher_s, developer = developer_s)

clean_df$owners <- as.factor(clean_df$owners)

# try many ways to order the factors, this one was the only that worked.
clean_df$owners_f <- factor(clean_df$owners,
                           levels(clean_df$owners)[c(1,7,11,4,9,13,2,6,10,3,8,12,5)])

clean_df %>% group_by(owners_f) %>% summarize(total = n()) %>% arrange(owners_f)

# process date ---------------------------------------------------

clean_df$release_date_as <- mdy(clean_df$release_date)


# Clean data: get observactions without missing data--------------

df <- clean_df[complete.cases(clean_df), ]


# Get metascore per game ordered by date -------------------------
df_games <- df %>% group_by(developer) %>% mutate(n_games = n())

df_10 <- df_games %>% filter(n_games >= 10)

df_10n <- df_10 %>% group_by(developer) %>% arrange(release_date_as) %>%
                    mutate(nth_game = row_number())


df_10n$developer <- as.factor(df_10n$developer)

df_10n$developer_f <- factor(df_10n$developer,
                           levels(df_10n$developer)[c(8,2,6,5,3,4,1,9,7)])


# Vis -----------------------------------------------------------
source(here("2019-07-30", "custom_theme.R"))
p <- ggplot(df_10n, aes(x = nth_game, y = metascore,
                  group = as.factor(developer_f),
                  color = as.factor(developer_f))) +
  geom_mark_circle(aes(label = "First game released", 
                       filter = developer == "Ubisoft Montreal" & nth_game == 1), 
                   expand = unit(3, "mm"), label.fill = "#f5f5f2", label.fontsize = 10) +
    geom_mark_circle(aes(label = "Last game", 
                       filter = developer == "Team17 Digital Ltd" & nth_game == 10), 
                   expand = unit(3, "mm"),label.fill = "#f5f5f2", label.fontsize = 10) +
              geom_line(size = 2) +
              facet_wrap(~developer_f) +
              custom_theme() +
  labs(title = "Do Developers make better games over time?",
       subtitle = "Changes in metascore by game release\n",
       caption = "Data: @brightcdns | Vis: @csmontt")

ggsave(here("figures", "2019-07-30.png"), plot = p, width = 10, height = 6)  


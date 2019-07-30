library(tidyverse)
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")


# goal difference as a measure of success, the higher, the more succesful the team?
# correlation between this, and cetrality network.

outcome_2019 <- wwc_outcomes %>% filter(year == 2019 & round == "Group")

outcome_2019$row_id <- 1:nrow(outcome_2019)
outcome_2019$even_rows <- outcome_2019$row_id %% 2

outcome_2019$new_row <- NA


new_0 <- which(outcome_2019$even_rows == 0) - 1
dta_0 <- outcome_2019$score[outcome_2019$even_rows == 0]
outcome_2019$new_row[new_0] <- dta_0

new_1 <- which(outcome_2019$even_rows == 1) + 1
dta_1 <- outcome_2019$score[outcome_2019$even_rows == 1]
outcome_2019$new_row[new_1] <- dta_1

outcome_2019$diff_goals <- outcome_2019$score - outcome_2019$new_row

team_goals <- outcome_2019 %>% group_by(team) %>% summarize(goals_diff = sum(diff_goals))



# use points in group stage to determine degree of succesfulness ----------------------
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
wwc_outcomes_filtered <- wwc_outcomes %>% filter(year == 2019 & round == "Group")
outcome_2019 <- wwc_outcomes_filtered %>% mutate(points = ifelse(win_status == "Won", 3,
                                                        ifelse(win_status == "Tie", 1,
                                                               0)))

team_points <- outcome_2019 %>% group_by(team) %>% summarise(total_points = sum(points))


# consider also until what stage they got?--------------------------------------------
filtered_stages <- wwc_outcomes %>% filter(year == 2019) 
stage <- filtered_stages  %>% select(team, round) %>% distinct() 

stage <- stage[stage$round != "Third Place Playoff", ]

stage <- stage %>% group_by(team) %>% summarize(rounds = n())


# combine all----------------------
team_stats <- left_join(team_points, team_goals)
team_stats <- left_join(team_stats, stage)


normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

team_stats$total_points2 <- normalize(team_stats$total_points)*0.5
team_stats$goals_diff2 <- normalize(team_stats$goals_diff)*2
team_stats$rounds2 <- normalize(team_stats$rounds)*2

team_stats$success <- team_stats$total_points2 + team_stats$goals_diff2 + team_stats$rounds2

team_stats$success <- normalize(team_stats$success)

team_stats <- left_join(codes, team_stats)

team_stats <- team_stats[complete.cases(team_stats), ]

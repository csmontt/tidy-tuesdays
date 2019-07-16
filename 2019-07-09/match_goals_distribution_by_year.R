wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")

# Goals per game distribution per world cup?

goals_wc <- wwc_outcomes %>% group_by(year, yearly_game_id) %>% 
        summarize(total_goals = sum(score))

p = ggplot(goals_wc, aes(x=total_goals)) +
    geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10) +
    facet_grid(. ~ year)


p


library(ggridges)
ggplot(goals_wc, aes(x = total_goals, y = as.factor(year))) + geom_density_ridges()




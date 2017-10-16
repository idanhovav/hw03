###
# title: Prepare Data
# description: this script takes the raw data and formats it properly.
# input: nba2017-rosters.csv, nba2017-stats.csv
# output:
###

setwd("Programming/Berk/stat133/stat133-hws-fall17/hw03/")
rosters <- read.csv("data/nba2017-roster.csv")
data <- read.csv("data/nba2017-stats.csv")

data <- data %>% mutate(missed_fg = data$field_goals_atts - data$field_goals_made)
data <- data %>% mutate(missed_ft = data$points1_atts - data$points1_made)
data <- data %>% mutate(points = data$points1_made + data$points2_made * 2 + data$points3_made * 3)
data <- data %>% mutate(rebounds = data$off_rebounds + data$def_rebounds)

data <- data %>% mutate(efficiency = (data$points + data$rebounds + data$assists + data$steals + data$blocks - data$missed_fg - data$missed_ft - data$turnovers) / data$games_played)

sink(file = 'output/efficiency-summary.txt')
summary(data$efficiency)
sink()

teams_data <- merge(rosters, data, by = c("player" = "player"))

teams <- data.frame(teams_data$team)

aggregate(teams_data$experience, teams_data$salary, by=list(team=teams_data$team), FUN=sum)


teams$experience <- teams_data %>% group_by(teams_data$team) %>% sum(teams_data$points3_made)



teams <- teams_data %>% group_by(team) %>% 
  summarize(experience = sum(experience), 
            salary = sum(salary),
            points3 = sum(points3_made),
            points2 = sum(points2_made),
            free_throws = sum(points1_made),
            points = sum(points),
            off_rebounds = sum(off_rebounds),
            def_rebounds = sum(def_rebounds),
            assists = sum(assists),
            steals = sum(steals),
            blocks = sum(blocks),
            turnovers = sum(turnovers),
            fouls = sum(fouls),
            efficiency = sum(efficiency)
          )


sink(file = 'data/teams-summary.txt')
summary(teams)
sink()

write.csv(teams, file = 'data/nba2017-teams.csv')

pdf('images/teams_star_plot.pdf')
stars(teams[ , -1], labels = as.vector(teams$team))
dev.off()

pdf('images/experience_salary.pdf')
ggplot(teams, aes(experience, salary)) + geom_point() + geom_text(aes(label=team))
dev.off()


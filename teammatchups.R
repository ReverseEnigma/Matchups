# load packages
library(tidyverse)
library(extrafont)
library(nbastatR)
library(httr)
library(jsonlite)
library(janitor)
library(hablar)
setwd("C:/Users/aniru/OneDrive/Documents/nba stuff/Matchups")
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# custom theme
theme_owen <- function () {
  theme_minimal(base_size=11, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}
# get NBA players and their IDs
players <- nba_players() %>% 
  select(namePlayer, idPlayer)


headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# create a dataframe with every combo of player, season, and season mode/stage
d1 <- expand.grid(offensivePlayerID = players$idPlayer[players$namePlayer == "Nikola Jokic"], 
                  season = c("2018-19","2019-20", "2020-21", "2021-22","2022-23","2023-24"),
                  #season = c("2019-20"),
                  seasonMode = c("Regular+Season", "Playoffs"), 
                  #seasonMode = c( "Playoffs"),
                  stringsAsFactors = FALSE)

# create a url string for our scraping function 
d1$url <- paste0("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&LeagueID=00&OffPlayerID=", d1$offensivePlayerID, "&Outcome=&PORound=0&PerMode=Totals&Season=", d1$season, "&SeasonType=", d1$seasonMode)

# create function to get data
get_data <- function(url) {
  
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  df <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  df <- df %>% clean_names() %>% retype()
  
  return(df)
  
}

# create function that returns NULL if there's no data to return
possibly_get_data <- possibly(get_data, otherwise = NULL)

# get matchup data
dat <- map_df(d1$url, possibly_get_data)

# Combine player points and team points, then calculate points per 100 possessions
df <- dat %>%
  group_by(def_player_name) %>%
  summarise(total_poss = sum(partial_poss), 
            combined_pts = sum(player_pts) + sum(team_pts)) %>%
  mutate(pts_per_100 = (combined_pts / total_poss) * 100) %>%
  filter(total_poss >= 100)  # Adjust the threshold as needed for statistical significance

# Update the ggplot code for the new data
df %>%
  ggplot(aes(x = pts_per_100, 
             y = fct_reorder(def_player_name, -pts_per_100), 
             size = total_poss, 
             fill = pts_per_100)) + 
  geom_point(alpha = .75, 
             shape = 21, 
             color = 'black') + 
  coord_cartesian(clip = 'off')  +
  scale_x_continuous(breaks = seq(20, 120, 10)) +
  scale_size_continuous(range = c(1, 10), 
                        breaks = seq(50, 350, 50), 
                        limits = c(50, 400)) +
  scale_fill_gradient2(low = "#0571b0", 
                       mid = "white",
                       high = "#ca0020", 
                       midpoint = median(df$pts_per_100), 
                       guide = "none") + 
  theme_owen() +
  labs(size = "Total Possessions", 
       title = "Total Team Points Per 100 Possessions When Nikola Jokic is Guarded", 
       subtitle = "Combining Jokic's and the team's points per 100 possessions against specific defenders (Minimum 75 possessions, Regular Season + Playoffs)", 
       x = "Total points per 100 possessions", 
       y = NULL)

# Save the updated plot
ggsave("Jokic_Team_Offense_vs_Defenders.png", width = 15, height = 15, dpi = 300, type = 'cairo')
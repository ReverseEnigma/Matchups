# load packages
library(tidyverse)
library(extrafont)
library(nbastatR)
library(httr)
library(jsonlite)
library(janitor)
library(hablar)
library(viridis)

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
d1 <- expand.grid(offensivePlayerID = players$idPlayer[players$namePlayer == "Jamal Murray"], 
                  season = c("2017-18","2018-19","2019-20", "2020-21", "2021-22","2022-23","2023-24"),
                  #season = c("2023-24"),
                  seasonMode = c("Regular+Season", "Playoffs"), 
                  #seasonMode = c("Playoffs"), 
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

# calculate pts per 100 possessions and efficiency metrics (minimum 50 possessions)
df <- dat %>%
  group_by(def_player_name) %>%
  summarise(partial_poss = sum(partial_poss, na.rm = TRUE),
            player_pts = sum(player_pts, na.rm = TRUE),
            fga = sum(matchup_fga, na.rm = TRUE),
            fgm = sum(matchup_fgm, na.rm = TRUE),
            fg3m = sum(matchup_fg3m, na.rm = TRUE),
            fta = sum(matchup_fta, na.rm = TRUE),
            ftm = sum(matchup_ftm, na.rm = TRUE)) %>%
  mutate(pts_per_100 = (player_pts / partial_poss) * 100,
         ts_pct = player_pts / (2 * (fga + 0.44 * fta))) %>%
  filter(partial_poss >= 100)

# make chart with TS% as color gradient using viridis
df %>%
  ggplot(aes(x = pts_per_100,
             y = fct_reorder(def_player_name, -pts_per_100),
             size = partial_poss,
             fill = ts_pct)) +  # use fill for TS%
  geom_point(alpha = .75,
             shape = 21,
             color = 'black') +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(breaks = seq(20, 75, 10)) +
  scale_size_continuous(range = c(1, 10),
                        breaks = seq(50, 350, 50),
                        limits = c(50, 400)) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent) +  # viridis color scale
  theme_owen() +
  theme(plot.title.position = 'plot',
        plot.title = element_text(face = 'bold', size = 11),
        plot.subtitle = element_text(face = 'italic', size = 6.5),
        plot.margin = margin(10, 10, 15, 10),
        axis.text.y = element_text(size = 6.5),
        legend.position = c(0.75, 0.75),  # adjust legend position
        legend.background = element_rect(fill = "floralwhite")) +
  labs(size = "Total Possessions",
       fill = "True Shooting %",
       title = "Jamal Murray's Points Per 100 Possessions When Matched Up With ___",
       subtitle = "Among players that guarded Murray at least 100 possessions between 2018 & 2024 (Regular Season + Playoffs)",
       x = "points per 100 possessions",
       y = NULL)

# save the plot
ggsave("Murray_matchups_with_ts.png", width = 10, height = 30, dpi = 300, type = 'cairo')
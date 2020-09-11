library(pacman)
pacman::p_load(tidyverse,
               ggplot2,
               nbastatR,
               teamcolors,
               rvest,
               extrafont,
               ggthemes,
               plotly)

loadfonts(quiet = TRUE)

teams_players_stats(seasons = 2010:2020, 
                    types = "player",
                    tables = "general",
                    modes = "Totals",
                    season_types = c("Regular Season", "Playoffs"))

# Creating a player-year df
# columns represent RS/PO TS% and minutes 
ts_comparison <- dataGeneralPlayers %>% 
  mutate(ts = ifelse(fga != 0 & fta !=0, pts / (2*fga + 0.88*fta), NA),
         typeSeason = dplyr::recode(typeSeason, "Regular Season" = "reg",
                                    "Playoffs" = "playoffs")) %>% 
  dplyr::select(namePlayer, typeSeason, slugSeason, ts, minutes, fga) %>% 
  distinct(namePlayer, typeSeason, slugSeason, .keep_all = T) %>%  # 2 players have duplicate rows; removed here
  pivot_wider(id_cols = c(namePlayer, slugSeason), 
              names_from = typeSeason, 
              values_from = c(ts, minutes, fga),
              values_fill = NA) %>% 
  mutate(ts_diff = ts_playoffs - ts_reg,
         fga_per36_playoffs = fga_playoffs / minutes_playoffs * 36)

# Plotting RS TS% against playoffs TS% 
filter(ts_comparison, minutes_reg >= 500 & minutes_playoffs >= 300) %>% 
  ggplot(aes(x = ts_reg, y = ts_playoffs)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# Highlighting Curry's performance
warriors_col <- filter(teamcolors, name == "Golden State Warriors")$primary
rockets_col <- filter(teamcolors, name == "Houston Rockets")$primary
filter(ts_comparison, minutes_reg >= 500 & minutes_playoffs >= 300) %>% 
  ggplot(aes(x = ts_reg, y = ts_playoffs)) +
  geom_point(aes(col = as.factor(ifelse(namePlayer == "James Harden", 1, 0)))) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_color_manual(values = c("grey", rockets_col)) +
  guides(col = FALSE) +
  theme_bw()

# 2020 playoffs standouts 
filter(ts_comparison, slugSeason == "2019-20") %>% 
  arrange(ts_diff) %>% 
  View()

# TS difference
bucks_col <- filter(teamcolors, name == "Milwaukee Bucks")$primary
rockets_col <- filter(teamcolors, name == "Houston Rockets")$primary
nuggets_col <- filter(teamcolors, name == "Denver Nuggets")$primary
ts_comparison$colors <- case_when(ts_comparison$namePlayer == "Khris Middleton" & ts_comparison$slugSeason == "2019-20" ~ "middleton",
                                  ts_comparison$namePlayer == "Robert Covington" & ts_comparison$slugSeason == "2019-20" ~ "covington",
                                  ts_comparison$namePlayer == "Jamal Murray" & ts_comparison$slugSeason == "2019-20" ~ "murray",
                                  TRUE ~ "normal")
            
ts_diff_plot <- filter(ts_comparison, minutes_playoffs >= 300) %>% 
  add_column(y = 0) %>% 
  ggplot(aes(y = fga_per36_playoffs, x = ts_diff, col = colors, label = paste(namePlayer, slugSeason))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("normal" = "grey", 
                                "middleton" = bucks_col,
                                "covington" = rockets_col,
                                "murray" = nuggets_col)) +
  guides(col = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text(aes(label = ifelse(colors != "normal", namePlayer, ""),
                y = fga_per36_playoffs + 0.7)) +
  labs(x = "Difference in TS% between regular season and playoffs",
       y = "Playoffs FGA/36",
       title = "Regular season vs playoffs TS% differential (2010-2020)",
       subtitle = "Players with >300 playoff minutes in a single playoffs")

ggplotly(ts_diff_plot)
  
ggsave("figures/middleton_ts.png", height = 5, width = 7)
#---------------------------------------
# Career TS% in RS vs PO for notable stars
#---------------------------------------
# Scraping career MVP vote shares 
url <- "https://www.basketball-reference.com/leaders/nba_mvp_shares.html"
webpage <- read_html(url)

# For some reason, I can't extract columns one by one; 
# all cells show up in a single vector
# probably a css selector issue
rank_data_html <- html_nodes(webpage, "td")
rank_data <- html_text(rank_data_html)

# Extracting values from the single vector to create dataframe
# I then manipulate the character vector of player names to 
# delete the * that designates HOF players
mvp_shares <- data.frame(namePlayer = rank_data[seq(2, length(rank_data), 3)],
                         shares = rank_data[seq(3, length(rank_data), 3)]) %>% 
  mutate(shares = as.numeric(as.character(shares)),
         namePlayer = as.character(namePlayer)) %>% 
  mutate(namePlayer = ifelse(str_detect(namePlayer, "\\*"), 
                             substring(namePlayer, 1, (nchar(namePlayer) - 2)),
                             substring(namePlayer, 1, (nchar(namePlayer) - 1))))

# Keeping only players with >0.25 shares
# some players are duplicated (dk why), so I use distinct()
mvp_shares <- filter(mvp_shares, shares >= 0.25) %>% 
  distinct()

# Importing career stats of those players 
players_careers(players = mvp_shares$namePlayer)

career_stats <- left_join(dataPlayerCareerTotalsRegularSeason,
                          dataPlayerCareerTotalsPostSeason,
                          by = "namePlayer",
                          suffix = c("_reg", "_playoffs")) %>% 
  filter(idPlayer_reg != 201607 & idPlayer_playoffs != 201607) %>% 
  mutate(ts_reg = ptsTotals_reg / (2*fgaTotals_reg + 0.88*ftaTotals_reg),
         ts_playoffs = ptsTotals_playoffs / (2*fgaTotals_playoffs + 0.88*ftaTotals_playoffs)) %>% 
  dplyr::select(namePlayer, ts_reg, ts_playoffs, everything())

ggplot(data = career_stats, aes(x = ts_reg, y = ts_playoffs)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

#--------------------------------------------------
# Shots data
teams_players_stats(seasons = 2020, 
                    types = "player", 
                    tables = "shot locations",
                    season_types = "Regular Season")


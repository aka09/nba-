library(pacman)
pacman::p_load(rvest,
               tidyverse,
               nbastatR,
               plotly,
               teamcolors)

teams_players_stats(seasons = 2016:2020,
                    types = "player",
                    tables = "general",
                    periods = 4,
                    measures = "Base",
                    season_types = "Playoffs",
                    modes = "Totals") 

# Creating a dataset with combined 4th quarter fga of last 5 seasons in 
# playoffs games 
q4_efg <- dataGeneralPlayers %>% 
  dplyr::select(namePlayer, slugSeason, fgm, fg3m, fga) %>% 
  pivot_wider(id_cols = namePlayer,
              names_from  = slugSeason,
              values_from = c(fgm, fg3m, fga),
              values_fill = 0) %>% 
  mutate(fgm_total = `fgm_2015-16` + `fgm_2016-17` + `fgm_2017-18` + `fgm_2018-19` + `fgm_2019-20`,
         fg3m_total = `fg3m_2015-16` + `fg3m_2016-17` + `fg3m_2017-18` + `fg3m_2018-19` + `fg3m_2019-20`,
         fga_total = `fga_2015-16` + `fga_2016-17` + `fga_2017-18` + `fga_2018-19` + `fga_2019-20`,
         efg = (fgm_total + 0.5 * fg3m_total) / fga_total) %>% 
  dplyr::select(namePlayer, efg, fga_total)

# 4th quarter shooting efficiency and volyme 
filter(q4_efg, fga_total >= 75) %>% 
ggplot(aes(x = fga_total, y = efg)) +
  geom_point()

# Creating a dataset for first 3 quarters 
teams_players_stats(seasons = 2016:2020,
                    types = "player",
                    tables = "general",
                    periods = 1:3,
                    measures = "Base",
                    season_types = "Playoffs",
                    modes = "Totals") 

# Fewer players played in the first three quarters, presumably
# because of garbage time in 4th quarters 
q13_efg <- dataGeneralPlayers %>% 
  dplyr::select(namePlayer, yearSeason, idPeriod, fgm, fg3m, fga) %>% 
  mutate_at(vars(yearSeason, idPeriod),
            as.character) %>% 
  unite("year_period", yearSeason, idPeriod) %>% 
  pivot_wider(id_cols = namePlayer,
              names_from  = year_period,
              values_from = c(fgm, fg3m, fga),
              values_fill = 0) %>% 
  mutate(fgm_total = rowSums(.[2:16]),
         fg3m_total = rowSums(.[17:31]),
         fga_total = rowSums(.[32:46]),
         efg = (fgm_total + 0.5 * fg3m_total) / fga_total) %>% 
  dplyr::select(namePlayer, efg, fga_total)

efg_merge <- left_join(q13_efg, q4_efg, 
                       by = "namePlayer",
                       suffix = c("_q13", "_q4"))

rockets_col <- filter(teamcolors, name == "Houston Rockets")$primary
nuggets_col <- filter(teamcolors, name == "Denver Nuggets")$primary
efg_merge$colors <- case_when(efg_merge$namePlayer == "Russell Westbrook" ~ "westbrook",
                              efg_merge$namePlayer == "Jamal Murray" ~ "murray",
                              TRUE ~ "normal")

efg_q4_plot <- filter(efg_merge, fga_total_q13 >= 150 & fga_total_q4 >= 100) %>% 
  ggplot(aes(x = efg_q13, y = efg_q4, label = namePlayer, col = colors)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_text(aes(label = ifelse(colors != "normal", namePlayer, ""),
                y = efg_q4 + 0.0075)) +
  theme_minimal() +
  labs(title = "Playoff EFG% in the first 3 quarters vs the 4th quarter",
       subtitle = "2016-2020",
       x = "EFG% in quarters 1-3",
       y = "EFG% in 4th quarter") +
  scale_color_manual(values = c("westbrook" = rockets_col, 
                                "murray" = nuggets_col,
                                "normal" = "grey")) +
  guides(col = FALSE)

ggsave("figures/q4_playoffs_efg.png", height = 5, width = 7)

ggplotly()

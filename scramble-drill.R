library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)
library(gt)
library(fontawesome)
library(ggimage)


options(repr.plot.width=15, repr.plot.height = 10)

df_games <- read_csv("~/nfl-big-data-bowl-2021/games.csv",
                     col_types = cols())

df_plays <- read_csv("~/nfl-big-data-bowl-2021/plays.csv",
                     col_types = cols())

df_players <- read_csv("~/nfl-big-data-bowl-2021/players.csv",
                       col_types = cols())

weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("~/nfl-big-data-bowl-2021/week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId"))

df_merged <- inner_join(df_merged,
                        df_tracking,
                        by = c("gameId" = "gameId",
                               "playId" = "playId"))

passArivalEvents <- c('pass_outcome_caught',
                      'pass_arrived',
                      'pass_outcome_incomplete',
                      'pass_outcome_interception',
                      'pass_outcome_touchdown')

df_distanceToFootball <- df_merged %>%
  
  #determining side of ball
  mutate(
    
    sideOfBall = ifelse(((team == "home") & (possessionTeam == homeTeamAbbr)) | ((team == "away") & (possessionTeam == visitorTeamAbbr)), "offense", "defense"),
    
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,visitorTeamAbbr, homeTeamAbbr))

nfl_dtf <- df_distanceToFootball %>%

  #grouping by game, play and frame
  group_by(gameId, playId, frameId) %>% 
  
  #checking if football reading is in frame
  mutate(footballInPlay = sum(displayName == "Football") > 0) %>% 
  
  #using only frames with football marked; some plays its missing
  filter(footballInPlay) %>% 
  
  #adding x and y location of football as columns
  mutate(xFootball = x[displayName == "Football"],
         yFootball = y[displayName == "Football"]) %>% 
  
  
  #ungrouping
  ungroup() %>% 
  
  #grouping by game and play
  group_by(gameId, playId) %>%
  
  #selecting frames that contain pass arrival events
  filter(event %in% passArivalEvents) %>%
  
  #selecting first frame with in case there are multiple
  filter(frameId == min(frameId)) %>% 
  
  #calculating distance to football
  mutate(
    
    distToFootballAtBallArrival = sqrt((x - xFootball) ^ 2 +
                                         (y - yFootball) ^ 2)
    
  )

# Create dataset with scramble plays only
scramble_drill <- nfl_dtf %>%
  filter(typeDropback == "SCRAMBLE_ROLLOUT_LEFT" | typeDropback == "SCRAMBLE_ROLLOUT_RIGHT") %>%
  filter(sideOfBall == "defense") %>%
  filter(passResult != "S") %>%
  filter(displayName != "Football") %>%
  filter(distToFootballAtBallArrival <= 2.5) %>%
  select(playDescription,  epa, typeDropback, passResult, displayName, distToFootballAtBallArrival, defensiveTeam) %>%
  group_by(defensiveTeam)

headshot_links <- c("https://a.espncdn.com/i/headshots/nfl/players/full/4038538.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/11366.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2510863.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/17435.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15612.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/12527.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3061106.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2982870.png",
                    "https://a.espncdn.com/i/headshots/nfl/players/full/3925358.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3051388.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15806.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/14297.png",
                    "https://a.espncdn.com/i/headshots/nfl/players/full/13975.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15812.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3052101.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3895429.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/12691.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2578378.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3054847.png",
                    "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2970694.png")

#find closest defenders on scramble drills
closest_defenders <- scramble_drill %>%
  select(playId, defensiveTeam, displayName, distToFootballAtBallArrival) %>%
  group_by(displayName) %>%
  filter(n() >= 3) %>% 
  summarize(MeanDist = mean(distToFootballAtBallArrival, na.rm = TRUE)) %>%
  mutate(AvgDist = round(MeanDist, digits = 2)) %>%
  arrange(AvgDist) %>%
  mutate(rank = dense_rank(AvgDist)) %>%
  head(n = 20) %>%
  mutate(head = headshot_links) %>%
  select(rank, displayName, head, AvgDist)

# Table for Closest Defenders
closest_defenders %>%
  gt() %>%
  cols_align(align = "center",
             columns = vars(AvgDist)) %>%
  tab_options(
    data_row.padding = px(2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(TRUE)
  ) %>%
  tab_header(
    title = md("Closest Defenders on Average"),
    subtitle = "On scramble plays"
  ) %>%
  cols_label(
    rank = "RK",
    displayName = "Name",
    head = "",
    AvgDist = "Distance"
  ) %>%
  opt_all_caps() %>%
  tab_options(
    table.background.color = "white",
    column_labels.background.color = "purple",
    #row.striping.background_color = "#e0e0e0"
  ) %>%
  opt_row_striping() %>%
  opt_table_font(
    font = list(
      google_font("Lato"),
      default_fonts()
    )
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(head)),
    fn = function(x){
      gt::web_image(x)
    }
  ) %>%
  data_color(
    columns = vars(AvgDist),
    colors = scales::col_numeric(
      palette = c("#3fc1c9", "white"),
      domain = NULL
    )
  )

# filter Scramble plays by team + add logos
team_scramble_stats <- scramble_drill %>%
  select(playId, defensiveTeam, displayName, epa) %>%
  group_by(defensiveTeam) %>%
  summarize(Mean = mean(epa, na.rm=TRUE)) %>%
  mutate(EPA = round(Mean, digits = 2)) %>%
  arrange(Mean) %>%
  mutate(logo = c("https://a.espncdn.com/i/teamlogos/nfl/500/lac.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/atl.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/det.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/mia.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/phi.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/hou.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/ten.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/sea.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/chi.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/cle.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/tb.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/no.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/min.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/gb.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/den.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/ari.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/nyj.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/ne.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/buf.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/jax.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/cin.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/was.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/dal.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/ind.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/oak.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/kc.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/pit.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/la.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/sf.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/bal.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/car.png",
                  "https://a.espncdn.com/i/teamlogos/nfl/500/nyg.png"))


asp_ratio <- 1.12

# Plot team stats
team_scramble_stats %>% 
  ggplot(aes(x = reorder(defensiveTeam, -Mean),
             y = Mean)) + 
  geom_bar(stat = 'identity', 
           fill = ifelse(team_scramble_stats$Mean < 0, "lightgreen", "firebrick2")) +
  coord_flip() +
  ggtitle("Average EPA on Scramble Drills by Team") +
  theme_bw() +
  theme(text = element_text(size=16)) +
  geom_text(aes(label = EPA),
            nudge_y = ifelse(team_scramble_stats$Mean < 0, 0.13, -0.13)) + 
  ggthemes::theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  xlab('Team') +
  ylab("Average EPA") +
  geom_image(aes(image = logo),
             size = 0.04,
             by = "width",
             asp = asp_ratio) +
  theme(aspect.ratio = 1/asp_ratio) 

# filter by player
player_scramble_stats <- scramble_drill %>%
  select(playId, defensiveTeam, displayName, epa, distToFootballAtBallArrival) %>%
  group_by(displayName) %>%
  filter(n() >= 3) %>% 
  summarize(Mean = mean(epa, na.rm = TRUE)) %>%
  mutate(EPA = round(Mean, digits = 2))  

# worst 20 players
bad_scramble_stats <- player_scramble_stats %>%
  arrange(desc(Mean)) %>% 
  head(n = 20)

bad_scramble_stats %>%
  ggplot(aes(x = reorder(displayName, Mean),
             y = Mean)) +
  geom_bar(stat = 'identity',
           color = "red",
           fill = "pink") +
  geom_text(aes(label = EPA),
            nudge_y = -0.06) + 
  coord_flip() +
  ggtitle("20 Worst Players in Scramble Drill") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  ylab('EPA') +
  xlab('Player')

# best 20 players
good_scramble_stats <- player_scramble_stats %>%
  arrange(desc(-Mean)) %>% 
  head(n = 20)

good_scramble_stats %>%
  ggplot(aes(x = reorder(displayName, -Mean),
             y = abs(Mean))) +
  geom_bar(stat = 'identity',
           color = "darkgreen",
           fill = "lightgreen") +
  geom_text(aes(label = EPA),
            nudge_y = -0.1) + 
  coord_flip() +
  ggtitle("20 Best Players in Scramble Drill") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  ylab('EPA') +
  xlab('Player')

save.image("scramble_data.RData")

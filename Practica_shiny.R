##############################################
# CARGA DE PAQUETES
##############################################

library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(plotly)

##############################################
# CARGA DE DATOS
##############################################

plays   <- read.csv("plays.csv")
games   <- read.csv("games.csv")
players <- read.csv("players.csv")
pff     <- read.csv("pffScoutingData.csv")

##############################################
# UNIÓN DE TABLAS
##############################################

pff_players <- pff %>%
  left_join(players, by = "nflId")

base_data <- plays %>%
  left_join(games, by = "gameId") %>%
  left_join(pff_players, by = c("gameId", "playId"))

game_choices <- games %>%
  mutate(label = paste0(visitorTeamAbbr, " @ ", homeTeamAbbr, " | ", gameDate)) %>%
  select(gameId, label)

team_choices <- sort(unique(c(games$homeTeamAbbr, games$visitorTeamAbbr)))

##############################################
# FUNCIONES AUXILIARES
##############################################

unique_plays <- function(df) {
  df %>% distinct(gameId, playId, .keep_all = TRUE)
}

clock_to_seconds <- function(clock) {
  parts <- strsplit(as.character(clock), ":")
  sapply(parts, function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
}

format_seconds <- function(seconds) {
  minutes <- floor(seconds / 60)
  secs    <- round(seconds %% 60)
  sprintf("%02d:%02d", minutes, secs)
}

ordinal_rank <- function(x) {
  suffix <- ifelse(
    x %% 100 %in% c(11, 12, 13), "th",
    ifelse(
      x %% 10 == 1, "st",
      ifelse(
        x %% 10 == 2, "nd",
        ifelse(x %% 10 == 3, "rd", "th")
      )
    )
  )
  paste0(x, suffix)
}

possession_time_by_team <- function(df) {
  
  plays_unique <- df %>%
    unique_plays() %>%
    mutate(
      clock_seconds = clock_to_seconds(gameClock),
      game_elapsed = case_when(
        quarter == 1 ~ 15 * 60 - clock_seconds,
        quarter == 2 ~ 15 * 60 + (15 * 60 - clock_seconds),
        quarter == 3 ~ 30 * 60 + (15 * 60 - clock_seconds),
        quarter == 4 ~ 45 * 60 + (15 * 60 - clock_seconds),
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(possessionTeam), !is.na(game_elapsed)) %>%
    arrange(game_elapsed, playId) %>%
    mutate(
      possession_change = possessionTeam != lag(possessionTeam, default = first(possessionTeam)),
      drive_id = cumsum(possession_change)
    )
  
  drive_summary <- plays_unique %>%
    group_by(drive_id, possessionTeam) %>%
    summarise(
      last_play_elapsed = last(game_elapsed),
      .groups = "drop"
    ) %>%
    arrange(drive_id) %>%
    mutate(
      previous_drive_end = lag(last_play_elapsed),
      start_time     = ifelse(row_number() == 1, 0, previous_drive_end),
      end_time       = ifelse(row_number() == n(), 60 * 60, last_play_elapsed),
      drive_duration = end_time - start_time,
      drive_duration = ifelse(drive_duration < 0, 0, drive_duration)
    )
  
  team_time <- drive_summary %>%
    group_by(possessionTeam) %>%
    summarise(
      possession_seconds = sum(drive_duration, na.rm = TRUE),
      possession_time    = format_seconds(possession_seconds),
      .groups = "drop"
    )
  
  total_time <- sum(team_time$possession_seconds, na.rm = TRUE)
  
  if (round(total_time) != 3600) {
    warning(paste0(
      "Time of possession does not sum to 60:00. Current total = ",
      format_seconds(total_time)
    ))
  }
  
  team_time
}

##############################################
# GAMES — FUNCIONES
##############################################

scoreboard_quarter <- function(df, home, away) {
  
  df_unique <- df %>%
    unique_plays() %>%
    arrange(gameId, playId) %>%
    mutate(
      home_delta      = preSnapHomeScore    - lag(preSnapHomeScore,    default = 0),
      away_delta      = preSnapVisitorScore - lag(preSnapVisitorScore, default = 0),
      scoring_quarter = lag(quarter, default = first(quarter)),
      home_delta      = ifelse(home_delta < 0, 0, home_delta),
      away_delta      = ifelse(away_delta < 0, 0, away_delta)
    )
  
  home_q <- df_unique %>%
    group_by(scoring_quarter) %>%
    summarise(points = sum(home_delta, na.rm = TRUE), .groups = "drop") %>%
    mutate(team = home)
  
  away_q <- df_unique %>%
    group_by(scoring_quarter) %>%
    summarise(points = sum(away_delta, na.rm = TRUE), .groups = "drop") %>%
    mutate(team = away)
  
  bind_rows(away_q, home_q) %>%
    pivot_wider(
      names_from  = scoring_quarter,
      values_from = points,
      values_fill = 0
    ) %>%
    mutate(T = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    select(Team = team, everything())
}

team_game_cards <- function(df, team) {
  
  team_df <- df %>%
    filter(possessionTeam == team) %>%
    unique_plays()
  
  third  <- team_df %>% filter(down == 3)
  fourth <- team_df %>% filter(down == 4)
  
  penalty_count <- team_df %>%
    filter(!is.na(penaltyYards), penaltyYards != 0) %>%
    nrow()
  
  penalty_total <- sum(team_df$penaltyYards, na.rm = TRUE)
  
  tibble(
    team           = team,
    total_jugadas  = nrow(team_df),
    third_down     = paste0(sum(third$playResult  >= third$yardsToGo,  na.rm = TRUE), " / ", nrow(third)),
    fourth_down    = paste0(sum(fourth$playResult >= fourth$yardsToGo, na.rm = TRUE), " / ", nrow(fourth)),
    penalizaciones = paste0(penalty_count, " (", penalty_total, " yds)")
  )
}

team_offense_summary <- function(df, team) {
  
  team_df <- df %>%
    filter(possessionTeam == team) %>%
    unique_plays()
  
  total_passing_yards <- sum(team_df$playResult, na.rm = TRUE)
  
  pass_attempts <- team_df %>%
    filter(passResult %in% c("C", "I", "IN")) %>%
    nrow()
  
  explosive_plays <- team_df %>%
    filter(playResult >= 20) %>%
    nrow()
  
  possession <- possession_time_by_team(df) %>%
    filter(possessionTeam == team)
  
  tibble(
    team                = team,
    total_passing_yards = total_passing_yards,
    yards_per_attempt   = round(total_passing_yards / max(pass_attempts, 1), 2),
    explosive_plays     = explosive_plays,
    touchdowns          = sum(str_detect(tolower(team_df$playDescription), "touchdown"), na.rm = TRUE),
    possession_seconds  = ifelse(nrow(possession) == 0, 0,       possession$possession_seconds),
    possession_time     = ifelse(nrow(possession) == 0, "00:00", possession$possession_time)
  )
}

team_defense_summary <- function(df, team) {
  
  plays_def <- df %>%
    filter(defensiveTeam == team) %>%
    unique_plays()
  
  dropbacks <- nrow(plays_def)
  
  pressure <- df %>%
    filter(defensiveTeam == team) %>%
    summarise(
      team      = team,
      pff_hurry = sum(pff_hurry == 1, na.rm = TRUE),
      pff_hit   = sum(pff_hit   == 1, na.rm = TRUE),
      pff_sack  = sum(pff_sack  == 1, na.rm = TRUE),
      .groups   = "drop"
    )
  
  interceptions <- plays_def %>%
    summarise(interceptions = sum(passResult == "IN", na.rm = TRUE)) %>%
    pull(interceptions)
  
  pressure_total <- pressure$pff_hurry + pressure$pff_hit + pressure$pff_sack
  
  pressure_rate <- ifelse(
    dropbacks == 0,
    0,
    round(pressure_total / dropbacks, 3)
  )
  
  pressure %>%
    mutate(
      pressure_rate = pressure_rate,
      interceptions = interceptions
    )
}

##############################################
# TEAM — FUNCIONES
##############################################

team_numeric_summary <- function(df, selected_team, side = c("team", "opponents")) {
  
  side <- match.arg(side)
  
  selected_games <- games %>%
    filter(homeTeamAbbr == selected_team | visitorTeamAbbr == selected_team) %>%
    select(gameId, homeTeamAbbr, visitorTeamAbbr)
  
  game_ids <- selected_games$gameId
  df_games <- df %>% filter(gameId %in% game_ids)
  
  if (side == "team") {
    offense_df     <- df_games %>% filter(possessionTeam == selected_team) %>% unique_plays()
    defense_df     <- df_games %>% filter(defensiveTeam  == selected_team)
    defense_unique <- defense_df %>% unique_plays()
  } else {
    offense_df     <- df_games %>% filter(possessionTeam != selected_team) %>% unique_plays()
    defense_df     <- df_games %>% filter(defensiveTeam  != selected_team)
    defense_unique <- defense_df %>% unique_plays()
  }
  
  points_by_game <- selected_games %>%
    rowwise() %>%
    mutate(
      home_points     = max(df_games$preSnapHomeScore[df_games$gameId    == gameId], na.rm = TRUE),
      away_points     = max(df_games$preSnapVisitorScore[df_games$gameId == gameId], na.rm = TRUE),
      selected_points = ifelse(homeTeamAbbr == selected_team, home_points, away_points),
      opponent_points = ifelse(homeTeamAbbr == selected_team, away_points, home_points)
    ) %>%
    ungroup()
  
  ppg <- if (side == "team") {
    mean(points_by_game$selected_points, na.rm = TRUE)
  } else {
    mean(points_by_game$opponent_points, na.rm = TRUE)
  }
  
  pass_attempts       <- offense_df %>% filter(passResult %in% c("C", "I", "IN")) %>% nrow()
  total_passing_yards <- sum(offense_df$playResult, na.rm = TRUE)
  total_passing_td    <- sum(str_detect(tolower(offense_df$playDescription), "touchdown"), na.rm = TRUE)
  interceptions       <- sum(offense_df$passResult == "IN", na.rm = TRUE)
  explosive_plays     <- offense_df %>% filter(playResult >= 20) %>% nrow()
  
  third       <- offense_df %>% filter(down == 3)
  third_made  <- sum(third$playResult  >= third$yardsToGo,  na.rm = TRUE)
  third_att   <- nrow(third)
  third_pct   <- 100 * third_made / max(third_att, 1)
  
  fourth      <- offense_df %>% filter(down == 4)
  fourth_made <- sum(fourth$playResult >= fourth$yardsToGo, na.rm = TRUE)
  fourth_att  <- nrow(fourth)
  fourth_pct  <- 100 * fourth_made / max(fourth_att, 1)
  
  possession_all <- df_games %>%
    group_split(gameId) %>%
    map_dfr(possession_time_by_team)
  
  if (side == "team") {
    possession_seconds <- possession_all %>%
      filter(possessionTeam == selected_team) %>%
      summarise(avg_seconds = mean(possession_seconds, na.rm = TRUE)) %>%
      pull(avg_seconds)
  } else {
    possession_seconds <- possession_all %>%
      filter(possessionTeam != selected_team) %>%
      summarise(avg_seconds = mean(possession_seconds, na.rm = TRUE)) %>%
      pull(avg_seconds)
  }
  
  if (is.na(possession_seconds)) possession_seconds <- 0
  
  penalty_count  <- offense_df %>% filter(!is.na(penaltyYards), penaltyYards != 0) %>% nrow()
  penalty_yards  <- sum(offense_df$penaltyYards, na.rm = TRUE)
  sacks          <- sum(defense_df$pff_sack  == 1, na.rm = TRUE)
  pressure_total <- sum(defense_df$pff_hurry == 1, na.rm = TRUE) +
    sum(defense_df$pff_hit   == 1, na.rm = TRUE) +
    sum(defense_df$pff_sack  == 1, na.rm = TRUE)
  pressure_rate  <- 100 * pressure_total / max(nrow(defense_unique), 1)
  
  tibble(
    Group = c(
      "Scoring",
      "Passing", "Passing", "Passing", "Passing", "Passing",
      "Efficiency", "Efficiency", "Efficiency", "Efficiency",
      "Control",
      "Discipline",
      "Defense", "Defense"
    ),
    Metric = c(
      "PPG",
      "Total Passing TD", "Interceptions", "Total Passing Yards",
      "Yards Per Attempt", "Explosive Plays",
      "3rd Down Efficiency", "3rd Down %",
      "4th Down Efficiency", "4th Down %",
      "Average Time of Possession",
      "Penalties (Yards)",
      "Sacks", "Pressure Rate"
    ),
    RawValue = c(
      round(ppg, 1),
      total_passing_td, interceptions, total_passing_yards,
      round(total_passing_yards / max(pass_attempts, 1), 2), explosive_plays,
      third_pct, third_pct,
      fourth_pct, fourth_pct,
      possession_seconds,
      penalty_yards,
      sacks, pressure_rate
    ),
    Display = c(
      round(ppg, 1),
      total_passing_td, interceptions, total_passing_yards,
      round(total_passing_yards / max(pass_attempts, 1), 2), explosive_plays,
      paste0(third_made,  " / ", third_att),  paste0(round(third_pct,  1), "%"),
      paste0(fourth_made, " / ", fourth_att), paste0(round(fourth_pct, 1), "%"),
      format_seconds(possession_seconds),
      paste0(penalty_count, " (", penalty_yards, " yds)"),
      sacks, paste0(round(pressure_rate, 1), "%")
    )
  )
}

team_full_summary <- function(df, selected_team, side = c("team", "opponents")) {
  
  side <- match.arg(side)
  
  summary_df <- team_numeric_summary(df = df, selected_team = selected_team, side = side)
  
  league_ranking <- map_dfr(team_choices, function(team) {
    team_numeric_summary(df = df, selected_team = team, side = side) %>%
      mutate(team = team)
  }) %>%
    group_by(Metric) %>%
    mutate(
      rank = ifelse(
        Metric == "Interceptions",
        rank(RawValue,  ties.method = "min"),
        rank(-RawValue, ties.method = "min")
      )
    ) %>%
    ungroup() %>%
    filter(team == selected_team) %>%
    select(Metric, rank)
  
  summary_df %>%
    left_join(league_ranking, by = "Metric") %>%
    mutate(Display = paste0(Display, " (", ordinal_rank(rank), ")")) %>%
    select(Group, Metric, RawValue, Value = Display, Rank = rank)
}

league_metric_ranking <- function(df, metric_selected) {
  
  ranking_df <- map_dfr(team_choices, function(team) {
    team_numeric_summary(df = df, selected_team = team, side = "team") %>%
      filter(Metric == metric_selected) %>%
      mutate(Team = team)
  })
  
  ranking_df %>%
    mutate(
      Rank = ifelse(
        Metric == "Interceptions",
        rank(RawValue,  ties.method = "min"),
        rank(-RawValue, ties.method = "min")
      )
    ) %>%
    arrange(Rank) %>%
    select(Rank, Team, Metric, Value = Display)
}

##############################################
# QB — FUNCIONES
##############################################

nfl_qb_rating <- function(completions, attempts, yards, touchdowns, interceptions) {
  
  if (attempts == 0) return(0)
  
  a <- ((completions / attempts) - 0.3) * 5
  b <- ((yards / attempts) - 3) * 0.25
  c <- (touchdowns / attempts) * 20
  d <- 2.375 - ((interceptions / attempts) * 25)
  
  a <- min(max(a, 0), 2.375)
  b <- min(max(b, 0), 2.375)
  c <- min(max(c, 0), 2.375)
  d <- min(max(d, 0), 2.375)
  
  round(((a + b + c + d) / 6) * 100, 1)
}

qb_matrix <- function(df, min_attempts = 0) {
  
  df_unique <- df %>%
    distinct(gameId, playId, nflId, .keep_all = TRUE) %>%
    mutate(pff_role_clean = str_to_lower(str_trim(as.character(pff_role)))) %>%
    filter(pff_role_clean == "pass")
  
  df_unique %>%
    group_by(displayName, officialPosition) %>%
    summarise(
      Snaps              = n_distinct(paste(gameId, playId)),
      completions        = sum(passResult == "C",               na.rm = TRUE),
      attempts           = sum(passResult %in% c("C","I","IN"), na.rm = TRUE),
      completion_pct     = round(100 * completions / max(attempts, 1), 1),
      passing_yards      = sum(playResult, na.rm = TRUE),
      passing_touchdowns = sum(str_detect(tolower(playDescription), "touchdown"), na.rm = TRUE),
      interceptions      = sum(passResult == "IN", na.rm = TRUE),
      sacks              = sum(passResult == "S",  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      qb_rating = nfl_qb_rating(
        completions, attempts, passing_yards, passing_touchdowns, interceptions
      )
    ) %>%
    ungroup() %>%
    filter(attempts >= min_attempts) %>%
    transmute(
      Player                   = displayName,
      Position                 = officialPosition,
      Snaps,
      `QB Rating`              = qb_rating,
      `Completions / Attempts` = paste0(completions, " / ", attempts),
      `Completion %`           = paste0(completion_pct, "%"),
      `Passing Yards`          = passing_yards,
      `Passing TD`             = passing_touchdowns,
      Interceptions            = interceptions,
      Sacks                    = sacks
    ) %>%
    arrange(desc(`QB Rating`))
}

##############################################
# TACTICS — FUNCIONES
##############################################

tactics_filtered_data <- function(df, offenseFormation, personnelO, personnelD,
                                  passCoverage, passCoverageType,
                                  quarter_filter, down_filter, yards_range) {
  
  out <- df %>%
    unique_plays() %>%
    filter(!is.na(passResult))
  
  if (offenseFormation  != "All") out <- out %>% filter(offenseFormation    == !!offenseFormation)
  if (personnelO        != "All") out <- out %>% filter(personnelO          == !!personnelO)
  if (personnelD        != "All") out <- out %>% filter(personnelD          == !!personnelD)
  if (passCoverage      != "All") out <- out %>% filter(pff_passCoverage    == !!passCoverage)
  if (passCoverageType  != "All") out <- out %>% filter(pff_passCoverageType == !!passCoverageType)
  if (quarter_filter    != "All") out <- out %>% filter(quarter == as.numeric(quarter_filter))
  if (down_filter       != "All") out <- out %>% filter(down    == as.numeric(down_filter))
  
  out %>%
    filter(yardsToGo >= yards_range[1], yardsToGo <= yards_range[2])
}

tactics_kpis <- function(df) {
  
  total_plays <- nrow(df)
  
  tibble(
    Plays          = total_plays,
    `Completion %` = paste0(round(100 * sum(df$passResult == "C",  na.rm = TRUE) / max(total_plays, 1), 1), "%"),
    `Sack %`       = paste0(round(100 * sum(df$passResult == "S",  na.rm = TRUE) / max(total_plays, 1), 1), "%"),
    `INT %`        = paste0(round(100 * sum(df$passResult == "IN", na.rm = TRUE) / max(total_plays, 1), 1), "%"),
    `Avg YTG`      = round(mean(df$yardsToGo, na.rm = TRUE), 1)
  )
}

tactics_matrix <- function(df) {
  
  df %>%
    group_by(offenseFormation, pff_passCoverage) %>%
    summarise(
      Plays          = n(),
      `Completion %` = round(100 * sum(passResult == "C",  na.rm = TRUE) / n(), 1),
      Sacks          = sum(passResult == "S",  na.rm = TRUE),
      INT            = sum(passResult == "IN", na.rm = TRUE),
      `Avg YTG`      = round(mean(yardsToGo, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(Plays))
}

tactics_down_summary <- function(df) {
  
  df %>%
    filter(down %in% c(1, 2, 3, 4)) %>%
    group_by(down) %>%
    summarise(
      Plays          = n(),
      Completions    = sum(passResult == "C",  na.rm = TRUE),
      Incompletions  = sum(passResult == "I",  na.rm = TRUE),
      Sacks          = sum(passResult == "S",  na.rm = TRUE),
      Interceptions  = sum(passResult == "IN", na.rm = TRUE),
      `Completion %` = round(100 * Completions / Plays, 1),
      .groups = "drop"
    ) %>%
    arrange(down)
}

tactics_coverage_summary <- function(df) {
  
  df %>%
    group_by(pff_passCoverage, pff_passCoverageType) %>%
    summarise(
      Plays          = n(),
      `Completion %` = round(100 * sum(passResult == "C",  na.rm = TRUE) / n(), 1),
      Sacks          = sum(passResult == "S",  na.rm = TRUE),
      INT            = sum(passResult == "IN", na.rm = TRUE),
      `Avg YTG`      = round(mean(yardsToGo, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(Plays))
}

##############################################
# COMPONENTES VISUALES
##############################################

comparison_row <- function(label, left_value, right_value, left_display = NULL, right_display = NULL) {
  
  left_num  <- as.numeric(left_value)
  right_num <- as.numeric(right_value)
  total     <- left_num + right_num
  
  if (is.na(total) || total == 0) {
    left_pct  <- 50
    right_pct <- 50
  } else {
    left_pct  <- round((left_num / total) * 100, 1)
    right_pct <- round((right_num / total) * 100, 1)
  }
  
  if (is.null(left_display))  left_display  <- left_value
  if (is.null(right_display)) right_display <- right_value
  
  div(
    class = "compare-row",
    div(
      class = "compare-values",
      div(class = "compare-left-value",  left_display),
      div(class = "compare-label",       label),
      div(class = "compare-right-value", right_display)
    ),
    div(
      class = "stacked-bar",
      div(class = "stacked-left",  style = paste0("width:", left_pct,  "%;")),
      div(class = "stacked-right", style = paste0("width:", right_pct, "%;"))
    )
  )
}

comparison_card <- function(title, left_team, right_team, rows) {
  div(
    class = "compare-card",
    div(
      class = "compare-header",
      div(class = "compare-team",  left_team),
      div(class = "compare-title", title),
      div(class = "compare-team",  right_team)
    ),
    rows
  )
}

##############################################
# INTERFAZ DE USUARIO
##############################################

ui <- page_navbar(
  
  title = "NFL Big Data Bowl 2023",
  
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f6f8;
        font-family: Arial, Helvetica, sans-serif;
      }

      .main-container {
        max-width: 1200px;
        margin: auto;
      }

      .score-card, .compare-card, .team-bars-card, .tactics-card {
        background: white;
        border-radius: 12px;
        padding: 26px;
        margin-top: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }

      .score-header {
        display: flex;
        justify-content: space-between;
        font-size: 16px;
        margin-bottom: 28px;
      }

      .score-row {
        display: grid;
        grid-template-columns: 1.2fr 1fr 0.3fr 1fr 1.2fr;
        align-items: center;
        text-align: center;
        margin-bottom: 28px;
      }

      .team-abbr {
        font-size: 30px;
        font-weight: 700;
      }

      .team-side {
        font-size: 16px;
        color: #333;
      }

      .score-number {
        font-size: 72px;
        font-weight: 800;
        color: #333;
      }

      .score-separator {
        font-size: 44px;
        font-weight: 700;
        color: #555;
      }

      .kpi-card, .tactics-kpi {
        background: white;
        border-radius: 10px;
        padding: 18px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        margin-bottom: 15px;
      }

      .kpi-title, .tactics-kpi-title {
        font-size: 12px;
        color: #666;
        text-transform: uppercase;
      }

      .kpi-team {
        font-size: 14px;
        color: #013369;
        font-weight: 700;
        margin-top: 4px;
      }

      .kpi-value, .tactics-kpi-value {
        font-size: 26px;
        font-weight: 800;
        color: #013369;
      }

      .section-title {
        margin-top: 25px;
        margin-bottom: 10px;
        font-weight: bold;
        color: #013369;
      }

      .compare-header {
        display: grid;
        grid-template-columns: 1fr 2fr 1fr;
        align-items: center;
        text-align: center;
        margin-bottom: 20px;
      }

      .compare-team {
        font-size: 28px;
        font-weight: 800;
      }

      .compare-title {
        font-size: 18px;
        font-weight: 700;
      }

      .compare-row, .team-bar-row {
        margin: 24px 0;
      }

      .compare-values, .team-bar-values {
        display: grid;
        grid-template-columns: 1fr 2fr 1fr;
        align-items: center;
        margin-bottom: 8px;
      }

      .compare-left-value, .team-bar-left {
        text-align: left;
        font-size: 20px;
      }

      .compare-right-value, .team-bar-right {
        text-align: right;
        font-size: 20px;
      }

      .compare-label, .team-bar-label {
        text-align: center;
        font-size: 20px;
      }

      .stacked-bar, .team-stacked-bar {
        display: flex;
        width: 100%;
        height: 12px;
        border-radius: 8px;
        overflow: hidden;
        background: #e6e6e6;
      }

      .stacked-left, .team-stacked-left {
        height: 100%;
        background: #c94c5a;
      }

      .stacked-right, .team-stacked-right {
        height: 100%;
        background: #42405f;
      }

      .metric-group-title {
        font-size: 22px;
        font-weight: 800;
        color: #013369;
        margin-top: 28px;
        margin-bottom: 10px;
      }

      .tactics-grid {
        display: grid;
        grid-template-columns: repeat(5, 1fr);
        gap: 14px;
        margin-bottom: 22px;
      }
    "))
  ),
  
  nav_panel(
    title = "Games",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Game filters",
        selectInput(
          "game",
          "Select game",
          choices = setNames(game_choices$gameId, game_choices$label)
        )
      ),
      
      div(
        class = "main-container",
        uiOutput("scoreboard"),
        h3(class = "section-title", "Team Summary"),
        uiOutput("team_cards"),
        h3(class = "section-title", "Offensive Comparison"),
        uiOutput("offense_comparison_ui"),
        h3(class = "section-title", "Defensive Comparison"),
        uiOutput("defense_comparison_ui")
      )
    )
  ),
  
  nav_panel(
    title = "Team",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Team filters",
        selectInput(
          "team_filter",
          "Select team",
          choices = team_choices
        ),
        selectInput(
          "trend_metric",
          "Select metric",
          choices = c(
            "PPG"                        = "PPG",
            "Total Passing TD"           = "Total Passing TD",
            "Interceptions"              = "Interceptions",
            "Total Passing Yards"        = "Total Passing Yards",
            "Yards Per Attempt"          = "Yards Per Attempt",
            "Explosive Plays"            = "Explosive Plays",
            "3rd Down %"                 = "3rd Down %",
            "4th Down %"                 = "4th Down %",
            "Average Time of Possession" = "Average Time of Possession",
            "Penalties (Yards)"          = "Penalties (Yards)",
            "Sacks"                      = "Sacks",
            "Pressure Rate"              = "Pressure Rate"
          )
        )
      ),
      
      div(
        class = "main-container",
        
        h3(class = "section-title", "Weekly Trend Metric"),
        div(
          class = "score-card",
          plotlyOutput("team_trend_plot")
        ),
        
        h3(class = "section-title", "Team vs Opponents"),
        uiOutput("team_bars"),
        
        br(),
        
        h3(class = "section-title", "League Ranking by Metric"),
        DTOutput("metric_ranking_table")
      )
    )
  ),
  
  nav_panel(
    title = "QB",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "QB filters",
        numericInput(
          "qb_min_attempts",
          "Minimum attempts",
          value = 75,
          min   = 0,
          step  = 1
        )
      ),
      
      div(
        class = "main-container",
        h3(class = "section-title", "QB Rating vs Completion %"),
        div(
          class = "score-card",
          plotlyOutput("qb_scatter_plot")
        ),
        h3(class = "section-title", "QB Matrix"),
        DTOutput("qb_matrix_table")
      )
    )
  ),
  
  nav_panel(
    title = "TACTICS",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Tactical filters",
        
        selectInput("t_offenseFormation", "Offense Formation",
                    choices = c("All", sort(unique(na.omit(base_data$offenseFormation))))),
        
        selectInput("t_personnelO", "Personnel O",
                    choices = c("All", sort(unique(na.omit(base_data$personnelO))))),
        
        selectInput("t_personnelD", "Personnel D",
                    choices = c("All", sort(unique(na.omit(base_data$personnelD))))),
        
        selectInput("t_coverage", "Pass Coverage",
                    choices = c("All", sort(unique(na.omit(base_data$pff_passCoverage))))),
        
        selectInput("t_coverageType", "Coverage Type",
                    choices = c("All", sort(unique(na.omit(base_data$pff_passCoverageType))))),
        
        selectInput("t_quarter", "Quarter",
                    choices = c("All", sort(unique(na.omit(base_data$quarter))))),
        
        selectInput("t_down", "Down",
                    choices = c("All", 1, 2, 3, 4)),
        
        sliderInput("t_yardsToGo", "Yards To Go",
                    min   = 0,
                    max   = max(base_data$yardsToGo, na.rm = TRUE),
                    value = c(0, max(base_data$yardsToGo, na.rm = TRUE)))
      ),
      
      div(
        class = "main-container",
        
        h3(class = "section-title", "Tactical Coaching Dashboard"),
        uiOutput("tactics_kpi_cards"),
        
        div(
          class = "tactics-card",
          h4("Yards per Play by Offense Formation"),
          plotlyOutput("formation_box_plot")
        ),
        
        div(
          class = "tactics-card",
          h4("Formation vs Coverage — Completion % Heatmap"),
          p(style = "font-size:12px; color:#666; margin-top:-8px; margin-bottom:12px;",
            "Each cell shows the completion % when a formation faced a coverage. Darker green = more successful passing plays."),
          plotlyOutput("formation_coverage_heatmap")
        ),
        
        div(
          class = "tactics-card",
          h4("Formation vs Coverage Matrix"),
          DTOutput("tactics_matrix_table")
        ),
        
        div(
          class = "tactics-card",
          h4("Down Situation Summary"),
          DTOutput("tactics_down_table")
        ),
        
        div(
          class = "tactics-card",
          h4("Coverage Performance"),
          DTOutput("tactics_coverage_table")
        )
      )
    )
  )
)

##############################################
# SERVIDOR — GAMES
##############################################

server <- function(input, output) {
  
  game_data <- reactive({
    base_data %>% filter(gameId == as.numeric(input$game))
  })
  
  game_info <- reactive({
    games %>% filter(gameId == as.numeric(input$game)) %>% slice(1)
  })
  
  teams <- reactive({
    info <- game_info()
    c(home = info$homeTeamAbbr, away = info$visitorTeamAbbr)
  })
  
  output$scoreboard <- renderUI({
    info       <- game_info()
    t          <- teams()
    score      <- scoreboard_quarter(df = game_data(), home = t["home"], away = t["away"])
    away_score <- score %>% filter(Team == t["away"]) %>% pull(T)
    home_score <- score %>% filter(Team == t["home"]) %>% pull(T)
    
    div(
      class = "score-card",
      div(
        class = "score-header",
        span(paste("NFL ·", info$gameDate)),
        span("Finished")
      ),
      div(
        class = "score-row",
        div(div(class = "team-abbr", t["away"]), div(class = "team-side", "Away")),
        div(class = "score-number",    away_score),
        div(class = "score-separator", "-"),
        div(class = "score-number",    home_score),
        div(div(class = "team-abbr", t["home"]), div(class = "team-side", "Home"))
      ),
      h3("Points by quarter"),
      DTOutput("scoreboard_table")
    )
  })
  
  output$scoreboard_table <- renderDT({
    scoreboard_quarter(df = game_data(), home = teams()["home"], away = teams()["away"]) %>%
      datatable(options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$team_cards <- renderUI({
    t    <- teams()
    away <- team_game_cards(game_data(), t["away"])
    home <- team_game_cards(game_data(), t["home"])
    
    make_cards <- function(data, side) {
      tagList(
        column(3, div(class = "kpi-card",
                      div(class = "kpi-title", "Total offensive passing plays"),
                      div(class = "kpi-team",  paste(data$team, "-", side)),
                      div(class = "kpi-value", data$total_jugadas))),
        column(3, div(class = "kpi-card",
                      div(class = "kpi-title", "3rd down completions"),
                      div(class = "kpi-team",  paste(data$team, "-", side)),
                      div(class = "kpi-value", data$third_down))),
        column(3, div(class = "kpi-card",
                      div(class = "kpi-title", "4th down completions"),
                      div(class = "kpi-team",  paste(data$team, "-", side)),
                      div(class = "kpi-value", data$fourth_down))),
        column(3, div(class = "kpi-card",
                      div(class = "kpi-title", "Penalties"),
                      div(class = "kpi-team",  paste(data$team, "-", side)),
                      div(class = "kpi-value", data$penalizaciones)))
      )
    }
    
    tagList(
      fluidRow(make_cards(away, "Away")),
      fluidRow(make_cards(home, "Home"))
    )
  })
  
  output$offense_comparison_ui <- renderUI({
    away <- team_offense_summary(game_data(), teams()["away"])
    home <- team_offense_summary(game_data(), teams()["home"])
    
    comparison_card(
      title      = "OFFENSIVE COMPARISON",
      left_team  = teams()["away"],
      right_team = teams()["home"],
      rows = tagList(
        comparison_row("Total passing yards", away$total_passing_yards, home$total_passing_yards),
        comparison_row("Yards per attempt",   away$yards_per_attempt,   home$yards_per_attempt),
        comparison_row("Explosive plays",     away$explosive_plays,     home$explosive_plays),
        comparison_row("Touchdowns",          away$touchdowns,          home$touchdowns),
        comparison_row(
          "Time of possession",
          away$possession_seconds, home$possession_seconds,
          left_display  = away$possession_time,
          right_display = home$possession_time
        )
      )
    )
  })
  
  output$defense_comparison_ui <- renderUI({
    away <- team_defense_summary(game_data(), teams()["away"])
    home <- team_defense_summary(game_data(), teams()["home"])
    
    comparison_card(
      title      = "DEFENSIVE COMPARISON",
      left_team  = teams()["away"],
      right_team = teams()["home"],
      rows = tagList(
        comparison_row("Hurries",       away$pff_hurry,       home$pff_hurry),
        comparison_row("Hits",          away$pff_hit,         home$pff_hit),
        comparison_row("Sacks",         away$pff_sack,        home$pff_sack),
        comparison_row(
          "Pressure Rate",
          away$pressure_rate, home$pressure_rate,
          left_display  = paste0(round(away$pressure_rate * 100, 1), "%"),
          right_display = paste0(round(home$pressure_rate * 100, 1), "%")
        ),
        comparison_row("Interceptions", away$interceptions, home$interceptions)
      )
    )
  })
  
  ##############################################
  # SERVIDOR — TEAM
  ##############################################
  
  output$team_trend_plot <- renderPlotly({
    
    req(input$team_filter, input$trend_metric)
    selected_team <- input$team_filter
    metric        <- input$trend_metric
    
    calc_metric <- function(df_plays, w, team) {
      df_plays <- df_plays %>% unique_plays()
      if (nrow(df_plays) == 0) return(NA_real_)
      
      switch(metric,
             "PPG" = {
               max(df_plays$preSnapHomeScore, df_plays$preSnapVisitorScore, na.rm = TRUE)
             },
             "Total Passing TD" = {
               sum(str_detect(tolower(df_plays$playDescription), "touchdown"), na.rm = TRUE)
             },
             "Interceptions" = {
               sum(df_plays$passResult == "IN", na.rm = TRUE)
             },
             "Total Passing Yards" = {
               sum(df_plays$playResult, na.rm = TRUE)
             },
             "Yards Per Attempt" = {
               att <- df_plays %>% filter(passResult %in% c("C", "I", "IN"))
               if (nrow(att) == 0) return(NA_real_)
               round(sum(df_plays$playResult, na.rm = TRUE) / nrow(att), 2)
             },
             "Explosive Plays" = {
               sum(df_plays$playResult >= 20, na.rm = TRUE)
             },
             "3rd Down %" = {
               third <- df_plays %>% filter(down == 3)
               if (nrow(third) == 0) return(NA_real_)
               round(100 * sum(third$playResult >= third$yardsToGo, na.rm = TRUE) / nrow(third), 1)
             },
             "4th Down %" = {
               fourth <- df_plays %>% filter(down == 4)
               if (nrow(fourth) == 0) return(NA_real_)
               round(100 * sum(fourth$playResult >= fourth$yardsToGo, na.rm = TRUE) / nrow(fourth), 1)
             },
             "Average Time of Possession" = {
               game_ids <- unique(df_plays$gameId)
               df_full  <- base_data %>% filter(gameId %in% game_ids)
               pos <- possession_time_by_team(df_full) %>% filter(possessionTeam == team)
               if (nrow(pos) == 0) return(NA_real_)
               pos$possession_seconds[1]
             },
             "Penalties (Yards)" = {
               sum(df_plays$penaltyYards, na.rm = TRUE)
             },
             "Sacks" = {
               sum(df_plays$passResult == "S", na.rm = TRUE)
             },
             "Pressure Rate" = {
               pff_team  <- base_data %>%
                 filter(gameId %in% unique(df_plays$gameId), defensiveTeam == team)
               dropbacks <- nrow(df_plays)
               if (dropbacks == 0) return(NA_real_)
               pressure <- sum(pff_team$pff_hurry == 1, na.rm = TRUE) +
                 sum(pff_team$pff_hit   == 1, na.rm = TRUE) +
                 sum(pff_team$pff_sack  == 1, na.rm = TRUE)
               round(100 * pressure / dropbacks, 1)
             },
             NA_real_
      )
    }
    
    semanas <- sort(unique(base_data$week))
    
    team_weekly <- map_dfr(semanas, function(w) {
      df_w <- base_data %>% filter(week == w, possessionTeam == selected_team)
      tibble(week = w, valor = calc_metric(df_w, w, selected_team))
    })
    
    league_weekly <- map_dfr(semanas, function(w) {
      df_w    <- base_data %>% filter(week == w)
      equipos <- unique(df_w$possessionTeam)
      vals <- map_dbl(equipos, function(t) {
        calc_metric(df_w %>% filter(possessionTeam == t), w, t)
      })
      tibble(week = w, league_avg = round(mean(vals, na.rm = TRUE), 2))
    })
    
    plot_data <- team_weekly %>% left_join(league_weekly, by = "week")
    
    plot_ly(plot_data, x = ~week) %>%
      add_trace(
        y = ~valor, type = "scatter", mode = "lines+markers",
        name   = selected_team,
        line   = list(color = "#013369", width = 3),
        marker = list(color = "#013369", size = 8)
      ) %>%
      add_trace(
        y = ~league_avg, type = "scatter", mode = "lines+markers",
        name   = "League avg",
        line   = list(color = "#c94c5a", width = 2, dash = "dash"),
        marker = list(color = "#c94c5a", size = 6)
      ) %>%
      layout(
        xaxis     = list(title = "Week", dtick = 1),
        yaxis     = list(title = metric),
        legend    = list(orientation = "h", x = 0, y = 1.1),
        hovermode = "x unified",
        margin    = list(t = 30)
      )
  })
  
  output$team_bars <- renderUI({
    
    req(input$team_filter)
    selected_team <- input$team_filter
    
    team_values <- team_full_summary(
      df = base_data, selected_team = selected_team, side = "team"
    ) %>% rename(TeamRaw = RawValue, TeamValue = Value)
    
    opponent_values <- team_full_summary(
      df = base_data, selected_team = selected_team, side = "opponents"
    ) %>% rename(OpponentRaw = RawValue, OpponentValue = Value)
    
    plot_data <- team_values %>%
      left_join(opponent_values %>% select(Metric, OpponentRaw, OpponentValue), by = "Metric")
    
    make_bar <- function(metric, team_raw, opp_raw, team_value, opp_value) {
      
      total <- as.numeric(team_raw) + as.numeric(opp_raw)
      
      if (is.na(total) || total == 0) {
        left_pct  <- 50
        right_pct <- 50
      } else {
        left_pct  <- round(team_raw / total * 100, 1)
        right_pct <- round(opp_raw  / total * 100, 1)
      }
      
      div(
        class = "team-bar-row",
        div(
          class = "team-bar-values",
          div(class = "team-bar-left",  team_value),
          div(class = "team-bar-label", metric),
          div(class = "team-bar-right", opp_value)
        ),
        div(
          class = "team-stacked-bar",
          div(class = "team-stacked-left",  style = paste0("width:", left_pct,  "%;")),
          div(class = "team-stacked-right", style = paste0("width:", right_pct, "%;"))
        )
      )
    }
    
    groups <- unique(plot_data$Group)
    
    div(
      class = "team-bars-card",
      tagList(
        lapply(groups, function(g) {
          group_data <- plot_data %>% filter(Group == g)
          tagList(
            div(class = "metric-group-title", g),
            lapply(seq_len(nrow(group_data)), function(i) {
              make_bar(
                metric     = group_data$Metric[i],
                team_raw   = group_data$TeamRaw[i],
                opp_raw    = group_data$OpponentRaw[i],
                team_value = group_data$TeamValue[i],
                opp_value  = group_data$OpponentValue[i]
              )
            })
          )
        })
      )
    )
  })
  
  output$metric_ranking_table <- renderDT({
    
    req(input$trend_metric)
    
    ranking_table <- league_metric_ranking(df = base_data, metric_selected = input$trend_metric)
    selected_team <- input$team_filter
    
    datatable(
      ranking_table,
      options  = list(pageLength = 32, dom = "t", paging = FALSE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Team",
        target          = "row",
        backgroundColor = styleEqual(selected_team, "#dce8f5"),
        fontWeight      = styleEqual(selected_team, "bold")
      )
  })
  
  ##############################################
  # SERVIDOR — QB
  ##############################################
  
  output$qb_scatter_plot <- renderPlotly({
    
    qb_data <- qb_matrix(df = base_data, min_attempts = input$qb_min_attempts) %>%
      mutate(
        completion_num = as.numeric(str_remove(`Completion %`, "%")),
        attempts_num   = as.numeric(str_extract(`Completions / Attempts`, "(?<=/ )\\d+"))
      ) %>%
      filter(attempts_num > 0, completion_num > 0, completion_num < 100, `QB Rating` > 0)
    
    plot_ly(
      qb_data,
      x    = ~completion_num,
      y    = ~`QB Rating`,
      size = ~attempts_num,
      text = ~paste0(
        "<b>", Player,        "</b><br>",
        "QB Rating: ",    `QB Rating`,    "<br>",
        "Completion %: ", `Completion %`, "<br>",
        "Attempts: ",     attempts_num,   "<br>",
        "Passing Yards: ", `Passing Yards`
      ),
      hoverinfo = "text",
      type      = "scatter",
      mode      = "markers",
      marker    = list(
        color    = "#013369",
        opacity  = 0.65,
        sizemode = "area",
        sizeref  = 0.08,
        line     = list(color = "white", width = 1)
      )
    ) %>%
      layout(
        xaxis     = list(title = "Completion %", range = c(30, 85),  zeroline = FALSE),
        yaxis     = list(title = "QB Rating",     range = c(30, 145), zeroline = FALSE),
        margin    = list(t = 20),
        hovermode = "closest"
      )
  })
  
  output$qb_matrix_table <- renderDT({
    
    req(input$qb_min_attempts)
    
    datatable(
      qb_matrix(df = base_data, min_attempts = input$qb_min_attempts),
      filter   = "top",
      options  = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  ##############################################
  # SERVIDOR — TACTICS
  ##############################################
  
  tactics_data <- reactive({
    tactics_filtered_data(
      df               = base_data,
      offenseFormation = input$t_offenseFormation,
      personnelO       = input$t_personnelO,
      personnelD       = input$t_personnelD,
      passCoverage     = input$t_coverage,
      passCoverageType = input$t_coverageType,
      quarter_filter   = input$t_quarter,
      down_filter      = input$t_down,
      yards_range      = input$t_yardsToGo
    )
  })
  
  output$tactics_kpi_cards <- renderUI({
    
    kpis <- tactics_kpis(tactics_data())
    
    div(
      class = "tactics-grid",
      div(class = "tactics-kpi",
          div(class = "tactics-kpi-title", "Plays"),
          div(class = "tactics-kpi-value", kpis$Plays)),
      div(class = "tactics-kpi",
          div(class = "tactics-kpi-title", "Completion %"),
          div(class = "tactics-kpi-value", kpis$`Completion %`)),
      div(class = "tactics-kpi",
          div(class = "tactics-kpi-title", "Sack %"),
          div(class = "tactics-kpi-value", kpis$`Sack %`)),
      div(class = "tactics-kpi",
          div(class = "tactics-kpi-title", "INT %"),
          div(class = "tactics-kpi-value", kpis$`INT %`)),
      div(class = "tactics-kpi",
          div(class = "tactics-kpi-title", "Avg YTG"),
          div(class = "tactics-kpi-value", kpis$`Avg YTG`))
    )
  })
  
  output$formation_box_plot <- renderPlotly({
    
    df <- tactics_data() %>%
      filter(!is.na(offenseFormation), !is.na(playResult))
    
    conteos <- df %>% count(offenseFormation) %>% filter(n >= 5)
    df      <- df %>% filter(offenseFormation %in% conteos$offenseFormation)
    
    orden_mediana <- df %>%
      group_by(offenseFormation) %>%
      summarise(med = median(playResult, na.rm = TRUE), .groups = "drop") %>%
      filter(offenseFormation != "JUMBO") %>%
      arrange(desc(med)) %>%
      pull(offenseFormation)
    
    orden <- c("JUMBO", orden_mediana)
    orden <- orden[orden %in% unique(df$offenseFormation)]
    
    df$offenseFormation <- factor(df$offenseFormation, levels = orden)
    
    n_formaciones <- length(orden)
    paleta <- colorRampPalette(c("#013369", "#3a6ea5", "#c94c5a", "#7a3040",
                                 "#4a4a8a", "#8a7a3a", "#3a7a5a"))(n_formaciones)
    
    plot_ly(
      df,
      x          = ~offenseFormation,
      y          = ~playResult,
      type       = "box",
      color      = ~offenseFormation,
      colors     = paleta,
      showlegend = FALSE,
      boxmean    = TRUE,
      hoverinfo  = "y+name"
    ) %>%
      layout(
        xaxis  = list(title = "Offense Formation"),
        yaxis  = list(title = "Yards per Play", zeroline = TRUE,
                      zerolinecolor = "#cccccc", zerolinewidth = 1),
        margin = list(t = 20)
      )
  })
  
  output$formation_coverage_heatmap <- renderPlotly({
    
    df <- tactics_data() %>%
      filter(!is.na(offenseFormation), !is.na(pff_passCoverage), !is.na(passResult))
    
    heat_data <- df %>%
      group_by(offenseFormation, pff_passCoverage) %>%
      summarise(
        plays          = n(),
        completions    = sum(passResult == "C", na.rm = TRUE),
        completion_pct = round(100 * completions / plays, 1),
        avg_yards      = round(mean(playResult, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      filter(plays >= 5)
    
    heat_matrix <- heat_data %>%
      select(offenseFormation, pff_passCoverage, completion_pct) %>%
      pivot_wider(names_from = pff_passCoverage, values_from = completion_pct) %>%
      column_to_rownames("offenseFormation")
    
    filas_resto <- setdiff(rownames(heat_matrix), "JUMBO")
    orden_filas <- c(filas_resto, "JUMBO")
    orden_filas <- orden_filas[orden_filas %in% rownames(heat_matrix)]
    heat_matrix <- heat_matrix[orden_filas, , drop = FALSE]
    
    cols_resto  <- setdiff(colnames(heat_matrix), "Goal Line")
    orden_cols  <- c(sort(cols_resto), "Goal Line")
    orden_cols  <- orden_cols[orden_cols %in% colnames(heat_matrix)]
    heat_matrix <- heat_matrix[, orden_cols, drop = FALSE]
    
    tooltip_matrix <- heat_data %>%
      mutate(tooltip = paste0(
        "<b>", offenseFormation, " vs ", pff_passCoverage, "</b><br>",
        "Completion: ", completion_pct, "%<br>",
        "Avg yards: ",  avg_yards,      "<br>",
        "Plays: ",      plays
      )) %>%
      select(offenseFormation, pff_passCoverage, tooltip) %>%
      pivot_wider(names_from = pff_passCoverage, values_from = tooltip) %>%
      column_to_rownames("offenseFormation")
    
    tooltip_matrix <- tooltip_matrix[rownames(heat_matrix), colnames(heat_matrix)]
    
    plot_ly(
      z         = as.matrix(heat_matrix),
      x         = colnames(heat_matrix),
      y         = rownames(heat_matrix),
      text      = as.matrix(tooltip_matrix),
      type      = "heatmap",
      hoverinfo = "text",
      colorscale = list(
        list(0,   "#c62828"),
        list(0.5, "#fff9c4"),
        list(1,   "#1b5e20")
      ),
      zmin     = 0,
      zmax     = 100,
      colorbar = list(title = "Completion %", ticksuffix = "%")
    ) %>%
      layout(
        xaxis  = list(title = "Pass Coverage", tickangle = -35),
        yaxis  = list(title = "Offense Formation"),
        margin = list(t = 20, b = 80)
      )
  })
  
  output$tactics_matrix_table <- renderDT({
    datatable(
      tactics_matrix(tactics_data()),
      filter   = "top",
      options  = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  output$tactics_down_table <- renderDT({
    datatable(
      tactics_down_summary(tactics_data()),
      options  = list(dom = "t", paging = FALSE, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$tactics_coverage_table <- renderDT({
    datatable(
      tactics_coverage_summary(tactics_data()),
      filter   = "top",
      options  = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
}

##############################################
# PONER SHINYAPP EN MARCHA
##############################################

shinyApp(ui, server)
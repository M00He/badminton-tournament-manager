# Tournament State — Schema, Konstruktoren, Mutationen, Serialisierung

SCHEMA_VERSION <- 2L

empty_players_df <- function() {
  data.frame(
    player_id = integer(),
    name      = character(),
    gender    = character(),   # "m" | "w"
    active    = logical(),
    stringsAsFactors = FALSE
  )
}

empty_games_df <- function() {
  data.frame(
    game_id = integer(), round = integer(), field = integer(),
    t1_p1 = integer(), t1_p2 = integer(), t2_p1 = integer(), t2_p2 = integer(),
    t1_set1 = integer(), t2_set1 = integer(),
    t1_set2 = integer(), t2_set2 = integer(),
    t1_set3 = integer(), t2_set3 = integer(),
    t1_points = integer(), t2_points = integer(),
    locked = logical(),
    stringsAsFactors = FALSE
  )
}

new_tournament_state <- function(name = NULL, created_at = NULL) {
  list(
    schema_version  = SCHEMA_VERSION,
    tournament_name = if (is.null(name)) "" else name,
    created_at      = if (is.null(created_at)) "" else created_at,
    settings        = list(num_rounds = 5L, num_fields = 4L,
                           game_system = "best_of_3_11"),
    status          = "setup",         # "setup" | "running" | "finished"
    current_round   = 1L,
    players         = empty_players_df(),
    games           = empty_games_df()
  )
}

.next_id <- function(ids) if (length(ids) == 0) 1L else max(ids) + 1L

ts_add_player <- function(state, name, gender) {
  name <- trimws(name)
  if (name == "") stop("Name darf nicht leer sein.")
  if (name %in% state$players$name) stop("Spieler existiert bereits.")
  if (!gender %in% c("m", "w")) stop("Geschlecht muss 'm' oder 'w' sein.")
  new_row <- data.frame(
    player_id = .next_id(state$players$player_id),
    name = name, gender = gender, active = TRUE,
    stringsAsFactors = FALSE
  )
  state$players <- rbind(state$players, new_row)
  state
}

ts_rename_player <- function(state, player_id, new_name, new_gender) {
  new_name <- trimws(new_name)
  if (new_name == "") stop("Name darf nicht leer sein.")
  idx <- which(state$players$player_id == player_id)
  if (length(idx) == 0) stop("Spieler nicht gefunden.")
  clash <- new_name %in% state$players$name[state$players$player_id != player_id]
  if (clash) stop("Name existiert bereits.")
  state$players$name[idx] <- new_name
  state$players$gender[idx] <- new_gender
  state
}

ts_set_player_active <- function(state, player_id, active) {
  idx <- which(state$players$player_id == player_id)
  if (length(idx) == 0) stop("Spieler nicht gefunden.")
  state$players$active[idx] <- isTRUE(active)
  state
}

ts_active_players <- function(state) {
  state$players[state$players$active %in% TRUE, , drop = FALSE]
}

ts_start_tournament <- function(state, num_rounds, num_fields, game_system) {
  if (nrow(ts_active_players(state)) < 4) stop("Mindestens 4 aktive Spieler benötigt.")
  state$settings <- list(num_rounds = as.integer(num_rounds),
                         num_fields = as.integer(num_fields),
                         game_system = game_system)
  state$current_round <- 1L
  state$status <- "running"
  state$games <- empty_games_df()
  state
}

ts_set_round_games <- function(state, round, pairings) {
  round <- as.integer(round)
  if (any(state$games$round == round & state$games$locked)) {
    stop("Runde ist gesperrt und kann nicht neu ausgelost werden.")
  }
  state$games <- state$games[state$games$round != round, , drop = FALSE]
  for (p in pairings) {
    row <- empty_games_df()[1, ]
    row$game_id <- .next_id(state$games$game_id)
    row$round <- round; row$field <- as.integer(p$field)
    row$t1_p1 <- p$team1[1]; row$t1_p2 <- p$team1[2]
    row$t2_p1 <- p$team2[1]; row$t2_p2 <- p$team2[2]
    row$locked <- FALSE
    state$games <- rbind(state$games, row)
  }
  state
}

# t1_sets/t2_sets: Länge-3-Vektoren (Best-of-3) ODER Länge-1 (Einzelsatz).
ts_save_result <- function(state, game_id, t1_sets, t2_sets) {
  idx <- which(state$games$game_id == game_id)
  if (length(idx) == 0) stop("Spiel nicht gefunden.")
  if (state$games$locked[idx]) stop("Spiel ist gesperrt.")
  sets <- sets_won_from_scores(t1_sets, t2_sets)
  state$games$t1_set1[idx] <- t1_sets[1]; state$games$t2_set1[idx] <- t2_sets[1]
  state$games$t1_set2[idx] <- if (length(t1_sets) >= 2) t1_sets[2] else NA_integer_
  state$games$t2_set2[idx] <- if (length(t2_sets) >= 2) t2_sets[2] else NA_integer_
  state$games$t1_set3[idx] <- if (length(t1_sets) >= 3) t1_sets[3] else NA_integer_
  state$games$t2_set3[idx] <- if (length(t2_sets) >= 3) t2_sets[3] else NA_integer_
  state$games$t1_points[idx] <- sets[1]
  state$games$t2_points[idx] <- sets[2]
  state
}

ts_lock_round <- function(state, round) {
  round <- as.integer(round)
  rows <- state$games$round == round
  if (!any(rows)) stop("Keine Spiele in dieser Runde.")
  if (any(is.na(state$games$t1_points[rows]) | is.na(state$games$t2_points[rows]))) {
    stop("Runde nicht abgeschlossen: es fehlen Ergebnisse.")
  }
  state$games$locked[rows] <- TRUE
  state
}

ts_advance_round <- function(state) {
  round <- state$current_round
  rows <- state$games$round == round
  if (!any(rows) || !all(state$games$locked[rows])) {
    stop("Aktuelle Runde nicht abgeschlossen.")
  }
  if (round >= state$settings$num_rounds) {
    state$status <- "finished"
  } else {
    state$current_round <- round + 1L
  }
  state
}

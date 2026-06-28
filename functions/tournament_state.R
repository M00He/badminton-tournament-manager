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
                           game_system = "best_of_3_11",
                           tiebreaker_order = "diff_first",
                           schedule_mode = "round_by_round"),
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

# Spieler entfernen: hat er noch kein Spiel, wird er ECHT gelöscht (Name wird wieder frei);
# hat er bereits gespielt, wird er nur inaktiv gesetzt (Historie bleibt erhalten).
ts_remove_player <- function(state, player_id) {
  in_games <- player_id %in% c(state$games$t1_p1, state$games$t1_p2,
                               state$games$t2_p1, state$games$t2_p2)
  if (isTRUE(in_games)) {
    idx <- which(state$players$player_id == player_id)
    if (length(idx)) state$players$active[idx] <- FALSE
  } else {
    state$players <- state$players[state$players$player_id != player_id, , drop = FALSE]
  }
  state
}

ts_active_players <- function(state) {
  state$players[state$players$active %in% TRUE, , drop = FALSE]
}

ts_start_tournament <- function(state, num_rounds, num_fields, game_system,
                                tiebreaker_order = "diff_first",
                                schedule_mode = "round_by_round",
                                plan_field_sequence = NULL) {
  if (nrow(ts_active_players(state)) < 4) stop("Mindestens 4 aktive Spieler benötigt.")
  stopifnot(tiebreaker_order %in% c("diff_first", "direct_first"))
  if (!schedule_mode %in% c("plan", "round_by_round"))
    stop("schedule_mode muss 'plan' oder 'round_by_round' sein.")
  if (identical(schedule_mode, "plan")) {
    if (is.null(plan_field_sequence) || length(plan_field_sequence) == 0)
      stop("Voraus-Plan benötigt eine Felder-Folge.")
    plan_field_sequence <- as.integer(plan_field_sequence)
    if (length(plan_field_sequence) < 2L)
      stop("Voraus-Plan braucht mindestens 2 Runden.")
    num_rounds <- length(plan_field_sequence)
    num_fields <- max(plan_field_sequence)
  } else {
    plan_field_sequence <- NULL
  }
  state$settings <- list(num_rounds = as.integer(num_rounds),
                         num_fields = as.integer(num_fields),
                         game_system = game_system,
                         tiebreaker_order = tiebreaker_order,
                         schedule_mode = schedule_mode,
                         plan_field_sequence = plan_field_sequence)
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

# Ergebnis in eine Spielzeile schreiben (gemeinsame Logik für Speichern & Korrigieren).
# t1_sets/t2_sets: Länge-3-Vektoren (Best-of-3) ODER Länge-1 (Einzelsatz).
.apply_result <- function(state, idx, t1_sets, t2_sets) {
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

ts_save_result <- function(state, game_id, t1_sets, t2_sets) {
  idx <- which(state$games$game_id == game_id)
  if (length(idx) == 0) stop("Spiel nicht gefunden.")
  if (state$games$locked[idx]) stop("Spiel ist gesperrt.")
  .apply_result(state, idx, t1_sets, t2_sets)
}

# Nachträgliche Korrektur eines Ergebnisses — auch in bereits abgeschlossenen (gesperrten) Runden.
ts_edit_result <- function(state, game_id, t1_sets, t2_sets) {
  idx <- which(state$games$game_id == game_id)
  if (length(idx) == 0) stop("Spiel nicht gefunden.")
  .apply_result(state, idx, t1_sets, t2_sets)
}

# Spieler eines Spiels setzen (manuelle Paarungs-Anpassung). Validiert: 4 verschiedene Spieler,
# keiner davon in einem ANDEREN Feld derselben Runde. Lock wird bewusst nicht geprüft —
# der Aufrufer entscheidet, ob editiert werden darf.
ts_set_game_players <- function(state, game_id, t1, t2) {
  idx <- which(state$games$game_id == game_id)
  if (length(idx) == 0) stop("Spiel nicht gefunden.")
  players <- c(t1, t2)
  if (any(is.na(players))) stop("Bitte 4 Spieler wählen.")
  if (length(unique(players)) != 4) stop("Jeder Spieler darf nur einmal pro Spiel.")
  rnd <- state$games$round[idx]
  others <- state$games[state$games$round == rnd & state$games$game_id != game_id, , drop = FALSE]
  used <- c(others$t1_p1, others$t1_p2, others$t2_p1, others$t2_p2)
  if (any(players %in% used)) stop("Ein Spieler ist bereits in einem anderen Feld dieser Runde.")
  state$games$t1_p1[idx] <- as.integer(t1[1]); state$games$t1_p2[idx] <- as.integer(t1[2])
  state$games$t2_p1[idx] <- as.integer(t2[1]); state$games$t2_p2[idx] <- as.integer(t2[2])
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

# ---- Serialisierung & Schema-Migration ---- #

state_to_json <- function(state) {
  jsonlite::toJSON(state, dataframe = "columns", null = "null",
                   na = "null", auto_unbox = TRUE, pretty = TRUE)
}

.as_players_df <- function(x) {
  if (is.null(x) || length(x) == 0) return(empty_players_df())
  data.frame(player_id = as.integer(x$player_id), name = as.character(x$name),
             gender = as.character(x$gender), active = as.logical(x$active),
             stringsAsFactors = FALSE)
}

.as_games_df <- function(x) {
  base <- empty_games_df()
  if (is.null(x) || length(x) == 0) return(base)
  cols <- names(base)
  df <- as.data.frame(lapply(cols, function(cn) {
    v <- x[[cn]]
    if (is.null(v)) return(rep(if (cn == "locked") NA else NA_integer_, length(x[[1]])))
    if (cn == "locked") as.logical(v) else as.integer(v)
  }), stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

migrate_state <- function(raw) {
  raw$players <- .as_players_df(raw$players)
  raw$games   <- .as_games_df(raw$games)
  raw$current_round <- as.integer(raw$current_round)
  raw$settings$num_rounds <- as.integer(raw$settings$num_rounds)
  raw$settings$num_fields <- as.integer(raw$settings$num_fields)
  if (is.null(raw$settings$tiebreaker_order)) raw$settings$tiebreaker_order <- "diff_first"
  if (is.null(raw$settings$schedule_mode)) raw$settings$schedule_mode <- "round_by_round"
  if (!is.null(raw$settings$plan_field_sequence))
    raw$settings$plan_field_sequence <- as.integer(raw$settings$plan_field_sequence)
  raw$schema_version <- SCHEMA_VERSION
  raw
}

state_from_json <- function(json) {
  raw <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  migrate_state(raw)
}

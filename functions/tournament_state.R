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
  state$players[isTRUE(state$players$active) | state$players$active, , drop = FALSE]
}

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

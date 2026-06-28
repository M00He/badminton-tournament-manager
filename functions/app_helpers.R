# App-Hilfsfunktionen (rein, ohne Shiny)

safe_filename <- function(s) {
  s <- gsub("[^[:alnum:]_-]", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  if (s == "") "turnier" else s
}

backup_filename <- function(state) {
  base <- if (is.null(state$tournament_name) || state$tournament_name == "") {
    "turnier"
  } else {
    safe_filename(state$tournament_name)
  }
  paste0(base, "_runde", state$current_round, ".json")
}

player_name <- function(state, id) {
  k <- which(state$players$player_id == id)
  if (length(k)) state$players$name[k] else "?"
}

# Verfügbare Spieler-IDs für einen Dropdown-Slot der manuellen Runde-1-Eingabe:
# alle aktiven IDs minus die in ANDEREN Slots gewählten; die eigene Wahl bleibt erhalten.
# all_ids und die Werte in `selections` sind character; `selections` ist nach Slot benannt.
slot_available_ids <- function(all_ids, selections, slot) {
  own <- selections[[slot]]
  own <- if (is.null(own)) "" else own
  others <- unlist(selections[setdiff(names(selections), slot)], use.names = FALSE)
  others <- others[!is.na(others) & nzchar(others)]
  all_ids[!(all_ids %in% others) | all_ids == own]
}

state_summary <- function(state) {
  status_label <- switch(state$status,
    setup = "Noch nicht gestartet",
    running = "Läuft",
    finished = "Abgeschlossen",
    state$status)
  name <- if (is.null(state$tournament_name) || state$tournament_name == "") {
    "(ohne Namen)"
  } else {
    state$tournament_name
  }
  list(name = name, round = state$current_round,
       num_rounds = state$settings$num_rounds,
       n_players = nrow(state$players), status_label = status_label)
}

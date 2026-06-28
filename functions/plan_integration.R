# Bruecke zwischen App-State und Spielplan-Generator (schedule_planner.R).
# Uebersetzt gespielte Runden in das Planner-Format und erzeugt die naechste
# garantiert-gueltige, an die aktuelle Tabelle re-optimierte Runde.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Eine gespielte Runde (aus state$games) ins Planner-Format list(field_count, games, byes).
games_round_to_plan <- function(state, round) {
  g <- state$games[state$games$round == round, , drop = FALSE]
  if (nrow(g) == 0) return(NULL)
  active <- ts_active_players(state)$player_id
  played <- c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2)
  byes <- as.integer(setdiff(active, played))
  games <- lapply(seq_len(nrow(g)), function(i) {
    x <- g[i, ]
    list(field = as.integer(x$field),
         team1 = c(as.integer(x$t1_p1), as.integer(x$t1_p2)),
         team2 = c(as.integer(x$t2_p1), as.integer(x$t2_p2)))
  })
  list(field_count = nrow(g), games = games, byes = byes)
}

# Alle Runden mit Spielen, aufsteigend = locked-prefix fuer den Generator.
played_rounds_as_plan <- function(state) {
  rounds <- sort(unique(state$games$round))
  out <- lapply(rounds, function(r) games_round_to_plan(state, r))
  out[!vapply(out, is.null, logical(1))]
}

# Staerke je Spieler aus der aktuellen Rangliste (hoeher = staerker).
strength_from_ranking <- function(state) {
  ids <- ts_active_players(state)$player_id
  if (length(ids) == 0) return(setNames(numeric(0), character(0)))
  r <- create_ranking(state$games, ids, state$settings$tiebreaker_order %||% "diff_first")
  n <- nrow(r)
  setNames(as.numeric(n - r$rank + 1L), as.character(r$player_id))  # Rang 1 -> hoechste Staerke
}

# Naechste Runde (= current_round) als garantiert-gueltige, an die Tabelle re-optimierte
# Fortsetzung des gespielten Praefix. Rueckgabe im Matchday-pairings-Format oder NULL.
plan_next_round_pairings <- function(state, seed = 1L, n_candidates = 300L) {
  fs <- state$settings$plan_field_sequence
  if (is.null(fs) || length(fs) == 0) return(NULL)
  players <- ts_active_players(state)$player_id
  k <- state$current_round
  if (k > length(fs)) return(NULL)
  played <- played_rounds_as_plan(state)
  base <- generate_schedule(players, fs, locked_rounds = played, seed = seed)
  if (is.null(base)) return(NULL)
  strength <- strength_from_ranking(state)
  full <- reoptimize_tail(players, fs, played_rounds = played, strength = strength,
                          current_schedule = base, n_candidates = n_candidates, seed = seed)
  rd <- full[[k]]
  if (is.null(rd)) return(NULL)
  list(pairings = rd$games, byes = rd$byes)
}

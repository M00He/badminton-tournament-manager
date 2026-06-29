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

# Gespielte Spiele je aktivem Spieler + bereits gespielte Partnerschaften unter Aktiven.
.dropout_play_info <- function(state, active) {
  cur <- setNames(integer(length(active)), as.character(active))
  used <- list()
  for (rd in played_rounds_as_plan(state)) for (gm in rd$games) {
    quad <- c(gm$team1, gm$team2)
    for (p in quad) if (p %in% active) cur[as.character(p)] <- cur[as.character(p)] + 1L
    for (tm in list(gm$team1, gm$team2)) {
      if (tm[1] %in% active && tm[2] %in% active) used[[length(used) + 1L]] <- c(tm[1], tm[2])
    }
  }
  list(cur = cur, used = used)
}

# Sucht G + Rest-Felder-Folge fuer die verbliebenen Spieler, so dass alle auf gleiche
# Gesamt-Spielzahl kommen und keine (auch keine bereits gespielte) Partnerschaft doppelt ist.
replan_after_dropout <- function(state, seed = 1L) {
  active <- ts_active_players(state)$player_id
  Pp <- length(active)
  if (Pp < 4L) return(NULL)
  Fmax <- as.integer(state$settings$num_fields)
  Feff <- min(Fmax, Pp %/% 4L)
  if (Feff < 1L) return(NULL)
  k <- state$current_round - 1L                       # gespielte Runden
  orig_fs <- as.integer(state$settings$plan_field_sequence)
  Gorig <- (sum(4L * orig_fs)) %/% (Pp + 1L)           # grobe Referenz (vor Austritt)

  info <- .dropout_play_info(state, active)
  cur <- info$cur
  used_count <- setNames(integer(Pp), as.character(active))
  for (p in info$used) {
    used_count[as.character(p[1])] <- used_count[as.character(p[1])] + 1L
    used_count[as.character(p[2])] <- used_count[as.character(p[2])] + 1L
  }

  cands <- list()
  for (G in seq.int(max(cur), Pp - 1L)) {
    total_add <- Pp * G - sum(cur)
    if (total_add <= 0L || total_add %% 4L != 0L) next
    if (any((G - cur) > ((Pp - 1L) - used_count))) next        # genug ungenutzte Partner?
    Sf <- total_add %/% 4L
    needR <- max(G - cur)
    Rp <- max(needR, as.integer(ceiling(Sf / Feff)))
    if (Rp < 1L || Sf < Rp) next                                # jede Runde >= 1 Feld
    q <- Sf %/% Rp; rem <- Sf %% Rp
    fs <- sort(c(rep(q + 1L, rem), rep(q, Rp - rem)), decreasing = TRUE)
    if (any(fs > Feff) || any(fs < 1L)) next
    cands[[length(cands) + 1L]] <- list(G = G, Rp = Rp, fs = fs, dist = abs(G - Gorig))
  }
  if (length(cands) == 0L) return(NULL)
  ord <- order(vapply(cands, function(cc) cc$dist, numeric(1)))
  for (j in ord) {
    cc <- cands[[j]]
    sched <- generate_schedule(active, cc$fs, init_games = cur,
                               forbidden_pairs = info$used, seed = seed)
    if (!is.null(sched))
      return(list(field_sequence = c(orig_fs[seq_len(k)], cc$fs), num_rounds = k + cc$Rp))
  }
  NULL
}

# Alle verbleibenden Runden (current_round..R) als garantiert-gueltige, an die Tabelle
# re-optimierte Fortsetzung. Rueckgabe: Liste von list(round, pairings, byes) oder NULL.
plan_remaining_rounds <- function(state, seed = 1L, n_candidates = 300L) {
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
  out <- lapply(k:length(fs), function(r) {
    rd <- full[[r]]
    if (is.null(rd)) return(NULL)
    list(round = r, pairings = rd$games, byes = rd$byes)
  })
  if (any(vapply(out, is.null, logical(1)))) return(NULL)  # gebrochener Rest -> NULL (kein stilles Verschieben der Runden)
  out
}

# Naechste Runde (= current_round) im Matchday-pairings-Format oder NULL.
plan_next_round_pairings <- function(state, seed = 1L, n_candidates = 300L) {
  rem <- plan_remaining_rounds(state, seed = seed, n_candidates = n_candidates)
  if (is.null(rem)) return(NULL)
  list(pairings = rem[[1]]$pairings, byes = rem[[1]]$byes)
}

# Spielplan-Generator: Feasibility, Konstruktion, Verifikation, Re-Optimierung.
# Reine Funktionen, kein Shiny/State, kein Echtzeit-Zufall (nur set.seed()).

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Prüft die harten Invarianten eines Plans (unabhängig vom Generator).
verify_schedule <- function(schedule, players) {
  P <- length(players)
  games_cnt <- setNames(integer(P), as.character(players))
  byes_cnt  <- setNames(integer(P), as.character(players))
  partner_seen <- list()
  errors <- character(0)

  for (r in seq_along(schedule)) {
    rd <- schedule[[r]]
    if (is.null(rd)) { errors <- c(errors, sprintf("Runde %d fehlt", r)); next }
    round_players <- integer(0)
    for (gm in rd$games) {
      round_players <- c(round_players, gm$team1, gm$team2)
      for (tm in list(gm$team1, gm$team2)) {
        key <- paste(sort(tm), collapse = "|")
        partner_seen[[key]] <- (partner_seen[[key]] %||% 0L) + 1L
      }
    }
    if (any(duplicated(round_players)))
      errors <- c(errors, sprintf("Runde %d: Spieler doppelt im Einsatz", r))
    gk <- as.character(round_players)
    games_cnt[gk] <- games_cnt[gk] + 1L
    if (length(rd$byes)) {
      bk <- as.character(rd$byes); byes_cnt[bk] <- byes_cnt[bk] + 1L
    }
    expected_bye <- setdiff(players, round_players)
    if (!setequal(rd$byes, expected_bye))
      errors <- c(errors, sprintf("Runde %d: Pausen stimmen nicht", r))
  }

  repeats <- names(partner_seen)[vapply(partner_seen, function(x) x > 1L, logical(1))]
  equal_games <- length(unique(games_cnt)) == 1L
  # equal_byes ist auch bei 0 Pausen je Spieler korrekt TRUE (alle gleich = 0)
  equal_byes  <- length(unique(byes_cnt)) == 1L
  ok <- length(errors) == 0L && length(repeats) == 0L && equal_games && equal_byes
  list(ok = ok, games_per_player = games_cnt, byes_per_player = byes_cnt,
       partner_repeats = repeats, equal_games = equal_games,
       equal_byes = equal_byes, errors = errors)
}

# Größtes G (Bedingung: P*G durch 4 teilbar -> Felder gehen auf), das in R Runden aufgeht.
# Hinweis: G muss NICHT gerade sein; bei P teilbar durch 4 ist ungerades G gueltig.
max_games_for <- function(P, F_max, R) {
  if (min(P - 1L, R) < 2L) return(0L)   # sonst liefe seq.int(2, <2) absteigend
  best <- 0L
  for (G in seq.int(2L, min(P - 1L, R), by = 1L)) {
    if ((P * G) %% 4L != 0L) next            # P*G/4 muss ganzzahlig sein
    S <- (P * G) %/% 4L                        # benötigte Feld-Summe
    if (S < R) next                            # jede Runde >= 1 Feld
    if (S > R * F_max) next                    # jede Runde <= F_max Felder
    best <- G
  }
  best
}

# Felder-Folge (absteigend) für R Runden; NULL falls infeasible.
field_sequence_for <- function(P, F_max, R) {
  G <- max_games_for(P, F_max, R)
  if (G == 0L) return(NULL)
  S <- (P * G) %/% 4L
  q <- S %/% R; rem <- S %% R
  fs <- c(rep(q + 1L, rem), rep(q, R - rem))    # Summe = S, jedes in {q, q+1}
  sort(fs, decreasing = TRUE)                    # mehr Felder zuerst
}

# Feasibility-Leiter: für jede sinnvolle Rundenzahl eine Option.
plan_options <- function(P, F_max, min_games = 4L, max_rounds = NULL) {
  if (is.null(max_rounds)) max_rounds <- P - 1L  # G <= P-1 ist die Obergrenze
  if (max_rounds < 2L) return(list())
  out <- list()
  for (R in seq.int(2L, max_rounds)) {
    G <- max_games_for(P, F_max, R)
    if (G < min_games) next
    fs <- field_sequence_for(P, F_max, R)
    if (is.null(fs)) next
    out[[length(out) + 1L]] <- list(rounds = R, games = G,
                                    byes = R - G, field_sequence = fs)
  }
  out
}

# Vorgeschlagene Rundenzahl: G möglichst in 6..8, bei Gleichstand wenige Pausen.
default_plan_rounds <- function(P, F_max) {
  opts <- plan_options(P, F_max)
  if (length(opts) == 0L) return(NA_integer_)
  score <- vapply(opts, function(o) {
    target <- if (o$games >= 6L && o$games <= 8L) 0L else min(abs(o$games - 6L), abs(o$games - 8L))
    target * 100L + o$byes                       # erst Ziel-Band, dann wenige Pausen
  }, integer(1))
  opts[[which.min(score)]]$rounds
}

# 1-Faktorisierung von K_P (P gerade) per Kreis-/Round-Robin-Methode.
# Liefert P-1 perfekte Paarungen; jedes Paar {i,j} kommt genau einmal vor.
circle_factorization <- function(P) {
  stopifnot(P %% 2L == 0L, P >= 2L)
  fixed <- P
  ring <- seq_len(P - 1L)            # rotierende Spieler
  rounds <- vector("list", P - 1L)
  for (r in seq_len(P - 1L)) {
    pairs <- list()
    pairs[[1]] <- c(fixed, ring[1])  # fester Spieler gegen Kopf des Rings
    half <- (P - 2L) %/% 2L
    for (i in seq_len(half)) {
      a <- ring[1L + i]
      b <- ring[length(ring) - i + 1L]
      pairs[[length(pairs) + 1L]] <- c(a, b)
    }
    rounds[[r]] <- pairs
    ring <- c(ring[length(ring)], ring[-length(ring)])  # um 1 rotieren
  }
  rounds
}

# Baut einen Plan aus einer Kreis-Faktorisierung (Sättigung: G = P-1, keine Pausen).
.schedule_from_circle <- function(players, field_sequence) {
  P <- length(players)
  R <- length(field_sequence)
  fac <- circle_factorization(P)                  # P-1 Runden Paarungen über 1..P
  rounds <- vector("list", R)
  for (r in seq_len(R)) {
    pairs <- fac[[r]]                              # P/2 Teams
    f <- field_sequence[r]
    if (length(pairs) != 2L * f) return(NULL)      # nur sauberer No-Bye-Fall
    games <- list()
    for (k in seq_len(f)) {
      t1 <- pairs[[2L * k - 1L]]; t2 <- pairs[[2L * k]]
      games[[k]] <- list(field = k,
                         team1 = players[t1], team2 = players[t2])
    }
    rounds[[r]] <- list(field_count = f, games = games, byes = integer(0))
  }
  rounds
}

# Randomisiert-konstruktiver Generator mit "muss-noch-spielen"-Regel + Neustarts.
generate_schedule <- function(players, field_sequence, locked_rounds = NULL,
                              seed = 1L, max_restarts = 2000L) {
  P <- length(players)
  R <- length(field_sequence)
  G <- (sum(4L * field_sequence)) %/% P
  idx <- seq_len(P)
  id_of <- players                                 # idx -> player_id
  to_idx <- function(id) match(id, id_of)
  n_locked <- if (is.null(locked_rounds)) 0L else length(locked_rounds)

  # Sättigungs-Sicherung: ohne Pausen und G = P-1 -> deterministisch via Kreis.
  no_byes <- all(field_sequence == P %/% 4L) && (P %% 4L == 0L)
  if (n_locked == 0L && G == P - 1L && no_byes) {
    sc <- .schedule_from_circle(players, field_sequence)
    if (!is.null(sc)) return(sc)
  }

  set.seed(seed)
  for (attempt in seq_len(max_restarts)) {
    partner_used <- matrix(FALSE, P, P)
    games_cnt <- integer(P); byes_cnt <- integer(P)
    rounds <- vector("list", R); ok <- TRUE

    if (n_locked > 0L) {
      for (r in seq_len(n_locked)) {
        lr <- locked_rounds[[r]]
        for (gm in lr$games) {
          a <- to_idx(gm$team1[1]); b <- to_idx(gm$team1[2])
          c <- to_idx(gm$team2[1]); d <- to_idx(gm$team2[2])
          partner_used[a, b] <- partner_used[b, a] <- TRUE
          partner_used[c, d] <- partner_used[d, c] <- TRUE
          games_cnt[c(a, b, c, d)] <- games_cnt[c(a, b, c, d)] + 1L
        }
        if (length(lr$byes)) {
          bi <- to_idx(lr$byes); byes_cnt[bi] <- byes_cnt[bi] + 1L
        }
        rounds[[r]] <- lr
      }
    }

    if (n_locked < R) for (r in (n_locked + 1L):R) {
      f <- field_sequence[r]; n_play <- 4L * f; n_bye <- P - n_play
      rem_rounds <- R - r + 1L
      need <- G - games_cnt
      must_play <- which(need >= rem_rounds)
      if (length(must_play) > n_play) { ok <- FALSE; break }
      cand_bye <- setdiff(idx, must_play)
      if (length(cand_bye) < n_bye) { ok <- FALSE; break }
      sitout <- cand_bye[order(byes_cnt[cand_bye], runif(length(cand_bye)))][seq_len(n_bye)]
      active <- sample(setdiff(idx, sitout))

      free <- active; teams <- list(); pair_ok <- TRUE
      while (length(free) >= 2L) {
        a <- free[1]; rest <- free[-1]
        compat <- rest[!partner_used[a, rest]]
        if (length(compat) == 0L) { pair_ok <- FALSE; break }
        b <- compat[sample.int(length(compat), 1L)]
        teams[[length(teams) + 1L]] <- c(a, b)
        free <- setdiff(free, c(a, b))
      }
      if (!pair_ok || length(teams) != 2L * f) { ok <- FALSE; break }

      games <- list()
      for (k in seq_len(f)) {
        t1 <- teams[[2L * k - 1L]]; t2 <- teams[[2L * k]]
        games[[k]] <- list(field = k, team1 = id_of[t1], team2 = id_of[t2])
        partner_used[t1[1], t1[2]] <- partner_used[t1[2], t1[1]] <- TRUE
        partner_used[t2[1], t2[2]] <- partner_used[t2[2], t2[1]] <- TRUE
      }
      games_cnt[active] <- games_cnt[active] + 1L
      byes_cnt[sitout] <- byes_cnt[sitout] + 1L
      rounds[[r]] <- list(field_count = f, games = games, byes = id_of[sitout])
    }

    if (ok && all(games_cnt == G) && all(byes_cnt == (R - G))) return(rounds)
  }
  NULL
}

# Stark+schwach-Strafe: +1 je Team, dessen beide Spieler auf derselben Median-Seite liegen.
schedule_balance_penalty <- function(schedule, strength, from_round = 1L) {
  med <- stats::median(strength)
  pen <- 0
  for (r in seq(from_round, length(schedule))) {
    rd <- schedule[[r]]; if (is.null(rd)) next
    for (gm in rd$games) {
      for (tm in list(gm$team1, gm$team2)) {
        s1 <- strength[as.character(tm[1])]; s2 <- strength[as.character(tm[2])]
        # Konvention: Spieler genau auf dem Median zaehlt zur "schwachen" Seite (<=)
        if (isTRUE((s1 <= med) == (s2 <= med))) pen <- pen + 1
      }
    }
  }
  pen
}

# Wählt unter gültigen Rest-Completions die beste für die aktuelle Tabelle.
# current_schedule ist immer Kandidat -> Ergebnis nie schlechter.
reoptimize_tail <- function(players, field_sequence, played_rounds, strength,
                            current_schedule, n_candidates = 300L, seed = 1L) {
  n_played <- length(played_rounds)
  best <- current_schedule
  best_pen <- schedule_balance_penalty(best, strength, from_round = n_played + 1L)
  for (i in seq_len(n_candidates)) {
    cand <- generate_schedule(players, field_sequence, locked_rounds = played_rounds,
                              seed = seed + i)
    if (is.null(cand)) next
    pen <- schedule_balance_penalty(cand, strength, from_round = n_played + 1L)
    if (pen < best_pen) { best <- cand; best_pen <- pen }
  }
  best
}

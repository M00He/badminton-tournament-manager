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

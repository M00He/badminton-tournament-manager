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
  equal_byes  <- length(unique(byes_cnt)) == 1L
  ok <- length(errors) == 0L && length(repeats) == 0L && equal_games && equal_byes
  list(ok = ok, games_per_player = games_cnt, byes_per_player = byes_cnt,
       partner_repeats = repeats, equal_games = equal_games,
       equal_byes = equal_byes, errors = errors)
}

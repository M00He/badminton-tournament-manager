# Rangliste — ID-basiert, Wertung nach gewonnenen Sätzen + konfigurierbarer Tiebreaker

calculate_player_stats <- function(games, player_ids) {
  n <- length(player_ids)
  stats <- data.frame(
    player_id = as.integer(player_ids), games_played = rep(0L, n),
    match_wins = rep(0L, n), match_losses = rep(0L, n),
    sets_won = rep(0L, n), sets_lost = rep(0L, n),
    rally_points_for = rep(0L, n), rally_points_against = rep(0L, n),
    rally_point_diff = rep(0L, n), stringsAsFactors = FALSE
  )
  if (nrow(games) == 0) return(stats)
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    t1_won <- g$t1_points > g$t2_points
    t1_rally <- sum(g$t1_set1, g$t1_set2, g$t1_set3, na.rm = TRUE)
    t2_rally <- sum(g$t2_set1, g$t2_set2, g$t2_set3, na.rm = TRUE)
    upd <- function(stats, ids, sets_w, sets_l, rally_f, rally_a, won) {
      for (id in ids) {
        k <- which(stats$player_id == id); if (!length(k)) next
        stats$games_played[k] <- stats$games_played[k] + 1L
        stats$sets_won[k] <- stats$sets_won[k] + sets_w
        stats$sets_lost[k] <- stats$sets_lost[k] + sets_l
        stats$rally_points_for[k] <- stats$rally_points_for[k] + rally_f
        stats$rally_points_against[k] <- stats$rally_points_against[k] + rally_a
        if (won) stats$match_wins[k] <- stats$match_wins[k] + 1L
        else stats$match_losses[k] <- stats$match_losses[k] + 1L
      }
      stats
    }
    stats <- upd(stats, t1, g$t1_points, g$t2_points, t1_rally, t2_rally, t1_won)
    stats <- upd(stats, t2, g$t2_points, g$t1_points, t2_rally, t1_rally, !t1_won)
  }
  stats$rally_point_diff <- stats$rally_points_for - stats$rally_points_against
  stats
}

get_direct_comparison <- function(id1, id2, games) {
  if (nrow(games) == 0) return(0L)
  w1 <- 0L; w2 <- 0L
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    opp <- (id1 %in% t1 && id2 %in% t2) || (id1 %in% t2 && id2 %in% t1)
    if (!opp) next
    t1_won <- g$t1_points > g$t2_points
    if ((id1 %in% t1 && t1_won) || (id1 %in% t2 && !t1_won)) w1 <- w1 + 1L else w2 <- w2 + 1L
  }
  if (w1 > w2) 1L else if (w2 > w1) -1L else 0L
}

create_ranking <- function(games, player_ids, tiebreaker_order = "diff_first") {
  stopifnot(tiebreaker_order %in% c("diff_first", "direct_first"))
  stats <- calculate_player_stats(games, player_ids)
  if (nrow(stats) == 0) { stats$rank <- integer(); return(stats) }
  # Basis-Ordnung: gewonnene Sätze, dann Punktedifferenz (stabiler Start)
  stats <- stats[order(-stats$sets_won, -stats$rally_point_diff), ]
  # Paarweise Verfeinerung: gibt TRUE, wenn Zeile a vor Zeile b stehen soll
  better <- function(a, b) {
    if (a$sets_won != b$sets_won) return(a$sets_won > b$sets_won)
    dc <- get_direct_comparison(a$player_id, b$player_id, games)
    diff_better <- a$rally_point_diff > b$rally_point_diff
    diff_equal  <- a$rally_point_diff == b$rally_point_diff
    if (tiebreaker_order == "diff_first") {
      if (!diff_equal) return(diff_better)
      return(dc > 0)
    } else {
      if (dc != 0) return(dc > 0)
      return(diff_better)
    }
  }
  n <- nrow(stats)
  if (n > 1) for (i in 1:(n - 1)) for (j in (i + 1):n) {
    if (better(stats[j, ], stats[i, ])) { tmp <- stats[i, ]; stats[i, ] <- stats[j, ]; stats[j, ] <- tmp }
  }
  stats$rank <- seq_len(nrow(stats))
  stats[, c("rank", "player_id", "games_played", "sets_won", "sets_lost",
            "match_wins", "match_losses", "rally_points_for",
            "rally_points_against", "rally_point_diff")]
}

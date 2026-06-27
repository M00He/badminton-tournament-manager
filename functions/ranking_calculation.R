# Rangliste — ID-basiert

calculate_player_stats <- function(games, player_ids) {
  stats <- data.frame(player_id = player_ids, games_played = 0L, wins = 0L,
                      losses = 0L, points_for = 0L, points_against = 0L,
                      point_diff = 0L, stringsAsFactors = FALSE)
  if (nrow(games) == 0) return(stats)
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    t1_won <- g$t1_points > g$t2_points
    upd <- function(stats, ids, pf, pa, won) {
      for (id in ids) {
        k <- which(stats$player_id == id); if (!length(k)) next
        stats$games_played[k] <- stats$games_played[k] + 1L
        stats$points_for[k]   <- stats$points_for[k] + pf
        stats$points_against[k] <- stats$points_against[k] + pa
        if (won) stats$wins[k] <- stats$wins[k] + 1L
        else stats$losses[k] <- stats$losses[k] + 1L
      }
      stats
    }
    stats <- upd(stats, t1, g$t1_points, g$t2_points, t1_won)
    stats <- upd(stats, t2, g$t2_points, g$t1_points, !t1_won)
  }
  stats$point_diff <- stats$points_for - stats$points_against
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

create_ranking <- function(games, player_ids) {
  stats <- calculate_player_stats(games, player_ids)
  if (nrow(stats) == 0) { stats$rank <- integer(); return(stats) }
  stats <- stats[order(-stats$wins, -stats$point_diff), ]
  n <- nrow(stats)
  if (n > 1) for (i in 1:(n - 1)) for (j in (i + 1):n) {
    if (stats$wins[i] == stats$wins[j] && stats$point_diff[i] == stats$point_diff[j]) {
      if (get_direct_comparison(stats$player_id[i], stats$player_id[j], games) < 0) {
        tmp <- stats[i, ]; stats[i, ] <- stats[j, ]; stats[j, ] <- tmp
      }
    }
  }
  stats$rank <- seq_len(nrow(stats))
  stats[, c("rank", "player_id", "games_played", "wins", "losses",
            "points_for", "points_against", "point_diff")]
}

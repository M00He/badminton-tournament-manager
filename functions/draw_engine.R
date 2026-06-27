# Auslosungs-Algorithmus (Score-and-Select)

.games_before <- function(games, before_round) {
  if (nrow(games) == 0) return(games)
  games[games$round < before_round, , drop = FALSE]
}

get_partnership_history <- function(games, before_round) {
  g <- .games_before(games, before_round); h <- list()
  push <- function(h, a, b) { k <- as.character(a); h[[k]] <- c(h[[k]], b); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]
    h <- push(h, r$t1_p1, r$t1_p2); h <- push(h, r$t1_p2, r$t1_p1)
    h <- push(h, r$t2_p1, r$t2_p2); h <- push(h, r$t2_p2, r$t2_p1)
  }
  h
}

get_opponent_history <- function(games, before_round) {
  g <- .games_before(games, before_round); h <- list()
  push <- function(h, a, opps) { k <- as.character(a); h[[k]] <- c(h[[k]], opps); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]; t1 <- c(r$t1_p1, r$t1_p2); t2 <- c(r$t2_p1, r$t2_p2)
    for (p in t1) h <- push(h, p, t2); for (p in t2) h <- push(h, p, t1)
  }
  h
}

get_opponent_team_history <- function(games, before_round) {
  g <- .games_before(games, before_round); h <- list()
  tid <- function(x) paste(sort(x), collapse = "|")
  push <- function(h, a, id) { k <- as.character(a); h[[k]] <- c(h[[k]], id); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]; t1 <- c(r$t1_p1, r$t1_p2); t2 <- c(r$t2_p1, r$t2_p2)
    for (p in t1) h <- push(h, p, tid(t2)); for (p in t2) h <- push(h, p, tid(t1))
  }
  h
}

get_previous_round_opponents <- function(games, round) {
  if (round <= 1) return(list())
  g <- games[games$round == (round - 1L), , drop = FALSE]; h <- list()
  push <- function(h, a, opps) { k <- as.character(a); h[[k]] <- c(h[[k]], opps); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]; t1 <- c(r$t1_p1, r$t1_p2); t2 <- c(r$t2_p1, r$t2_p2)
    for (p in t1) h <- push(h, p, t2); for (p in t2) h <- push(h, p, t1)
  }
  h
}

count_games_played <- function(games, player_ids, before_round) {
  counts <- setNames(rep(0L, length(player_ids)), as.character(player_ids))
  g <- .games_before(games, before_round)
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]
    for (id in c(r$t1_p1, r$t1_p2, r$t2_p1, r$t2_p2)) {
      k <- as.character(id)
      if (k %in% names(counts)) counts[k] <- counts[k] + 1L
    }
  }
  counts
}

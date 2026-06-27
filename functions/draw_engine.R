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

select_round_players <- function(state, round, ranking) {
  active <- ts_active_players(state)$player_id
  n_cap <- state$settings$num_fields * 4L
  n_play <- min(length(active), n_cap)
  n_play <- (n_play %/% 4L) * 4L
  gp <- count_games_played(state$games, active, before_round = round)
  rank_of <- function(id) {
    k <- which(ranking$player_id == id)
    if (length(k)) ranking$rank[k] else 9999L
  }
  ord <- order(gp[as.character(active)], vapply(active, rank_of, integer(1)))
  playing <- active[ord][seq_len(n_play)]
  byes <- setdiff(active, playing)
  list(playing = playing, byes = byes)
}

generate_candidate <- function(players, better_half, worse_half, num_fields) {
  better <- sample(intersect(better_half, players))
  worse  <- sample(intersect(worse_half, players))
  # Auffüllen, falls Hälften unsymmetrisch (z. B. durch Aussetzer)
  pool <- sample(players)
  take <- function(vec, n) { out <- vec[seq_len(n)]; out }
  pairings <- list()
  bi <- 1L; wi <- 1L
  for (f in seq_len(num_fields)) {
    quad <- c(better[bi], worse[wi], better[bi + 1L], worse[wi + 1L])
    bi <- bi + 2L; wi <- wi + 2L
    if (any(is.na(quad))) {                 # Fallback: aus Restpool ziehen
      used <- unlist(lapply(pairings, function(p) c(p$team1, p$team2)))
      rest <- setdiff(pool, c(used, quad[!is.na(quad)]))
      quad[is.na(quad)] <- rest[seq_len(sum(is.na(quad)))]
    }
    pairings[[f]] <- list(field = f, team1 = c(quad[1], quad[2]),
                          team2 = c(quad[3], quad[4]))
  }
  pairings
}

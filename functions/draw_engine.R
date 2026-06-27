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
  stopifnot(length(players) >= 4L * num_fields)
  better <- sample(intersect(better_half, players))
  worse  <- sample(intersect(worse_half, players))
  # Auffüllen, falls Hälften unsymmetrisch (z. B. durch Aussetzer)
  pool <- sample(players)
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

# Gewichte: Hierarchie über Größenordnungen (höhere Prio dominiert immer)
.W_PARTNER <- 1e5; .W_PREV <- 1e3; .W_TEAM <- 1e2; .W_OPP <- 1e1; .W_BALANCE <- 1

score_draw <- function(pairings, histories, ranking) {
  partner <- histories$partner; prev <- histories$prev
  team <- histories$team; opp <- histories$opp
  rank_of <- function(id) { k <- which(ranking$player_id == id)
    if (length(k)) ranking$rank[k] else 9999L }
  pen <- 0; viol <- c(partner = FALSE, prev = FALSE, team = FALSE, opp = FALSE, balance = FALSE)
  tid <- function(x) paste(sort(x), collapse = "|")
  med <- stats::median(ranking$rank)
  for (p in pairings) {
    t1 <- p$team1; t2 <- p$team2
    in_hist <- function(h, a, b) !is.null(h[[as.character(a)]]) && b %in% h[[as.character(a)]]
    # Prio 1: Partner
    if (in_hist(partner, t1[1], t1[2])) { pen <- pen + .W_PARTNER; viol["partner"] <- TRUE }
    if (in_hist(partner, t2[1], t2[2])) { pen <- pen + .W_PARTNER; viol["partner"] <- TRUE }
    # Prio 3: Gegner aus Vorrunde
    for (a in t1) for (b in t2) if (in_hist(prev, a, b)) { pen <- pen + .W_PREV; viol["prev"] <- TRUE }
    # Prio 4: Gegner-Team
    for (a in t1) if (!is.null(team[[as.character(a)]]) && tid(t2) %in% team[[as.character(a)]]) {
      pen <- pen + .W_TEAM; viol["team"] <- TRUE }
    # Prio 5: Einzelgegner
    for (a in t1) for (b in t2) if (in_hist(opp, a, b)) { pen <- pen + .W_OPP; viol["opp"] <- TRUE }
    # Prio 2: stark+schwach (jedes Team soll einen über und einen unter dem Median haben)
    bal_ok <- function(tm) (rank_of(tm[1]) <= med) != (rank_of(tm[2]) <= med)
    if (!bal_ok(t1)) { pen <- pen + .W_BALANCE; viol["balance"] <- TRUE }
    if (!bal_ok(t2)) { pen <- pen + .W_BALANCE; viol["balance"] <- TRUE }
  }
  prio_names <- c(partner = "Keine Partner-Wiederholung", prev = "Neue Gegner vs. Vorrunde",
                  team = "Neue Gegner-Teams", opp = "Neue Einzelgegner",
                  balance = "Stark/Schwach gepaart")
  list(penalty = pen, satisfied = unname(prio_names[!viol]))
}

generate_round_draw <- function(state, round, seed = 1L, n_candidates = 300L) {
  set.seed(seed)
  active_ids <- ts_active_players(state)$player_id
  ranking <- create_ranking(state$games, active_ids)
  # Sicherstellen: jede aktive ID hat einen Rang
  missing <- setdiff(active_ids, ranking$player_id)
  if (length(missing)) ranking <- rbind(ranking[, c("rank","player_id")],
    data.frame(rank = max(c(ranking$rank, 0L)) + seq_along(missing), player_id = missing))
  sel <- select_round_players(state, round, ranking)
  players <- sel$playing
  if (length(players) < 4) return(NULL)
  ranks <- ranking[match(players, ranking$player_id), ]
  ord <- players[order(ranks$rank)]
  mid <- length(ord) %/% 2L
  better_half <- ord[seq_len(mid)]; worse_half <- ord[(mid + 1L):length(ord)]
  histories <- list(
    partner = get_partnership_history(state$games, round),
    prev    = get_previous_round_opponents(state$games, round),
    team    = get_opponent_team_history(state$games, round),
    opp     = get_opponent_history(state$games, round)
  )
  num_fields <- length(players) %/% 4L
  best <- NULL; best_pen <- Inf; best_q <- NULL
  for (i in seq_len(n_candidates)) {
    cand <- generate_candidate(players, better_half, worse_half, num_fields)
    sc <- score_draw(cand, histories, ranking)
    if (sc$penalty < best_pen) { best <- cand; best_pen <- sc$penalty; best_q <- sc$satisfied
      if (best_pen == 0) break }
  }
  list(pairings = best, byes = sel$byes, penalty = best_pen, quality = best_q)
}

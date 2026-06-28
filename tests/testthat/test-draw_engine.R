source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")

two_round_games <- function() {
  g <- empty_games_df()
  add <- function(g, gid, rnd, fld, a, b, c, d) rbind(g, transform(empty_games_df()[1, ],
    game_id = gid, round = rnd, field = fld,
    t1_p1 = a, t1_p2 = b, t2_p1 = c, t2_p2 = d,
    t1_points = 2L, t2_points = 0L, locked = TRUE))
  g <- add(g, 1L, 1L, 1L, 1L, 2L, 3L, 4L)
  g <- add(g, 2L, 1L, 2L, 5L, 6L, 7L, 8L)
  g
}

test_that("get_partnership_history erfasst Partner beidseitig", {
  h <- get_partnership_history(two_round_games(), before_round = 99L)
  expect_equal(h[["1"]], 2L)
  expect_equal(h[["2"]], 1L)
})

test_that("count_games_played zählt nur Runden < before_round", {
  c1 <- count_games_played(two_round_games(), 1:8, before_round = 2L)
  expect_equal(unname(c1["1"]), 1L)
  c0 <- count_games_played(two_round_games(), 1:8, before_round = 1L)
  expect_equal(unname(c0["1"]), 0L)
})

test_that("get_previous_round_opponents liefert Gegner aus round-1", {
  h <- get_previous_round_opponents(two_round_games(), round = 2L)
  expect_setequal(h[["1"]], c(3L, 4L))
})

test_that("select_round_players bevorzugt Spieler mit wenigsten Spielen", {
  s <- new_tournament_state()
  for (i in 1:6) s <- ts_add_player(s, paste("P", i), "m")
  s <- ts_start_tournament(s, 5L, 1L, "best_of_3_11")  # 1 Feld => 4 spielen, 2 byes
  # P1..P4 haben in Runde 1 gespielt:
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L,11L), c(5L,7L))
  s <- ts_lock_round(s, 1L)
  rk <- create_ranking(s$games, ts_active_players(s)$player_id)
  sel <- select_round_players(s, round = 2L, ranking = rk)
  expect_length(sel$playing, 4L)
  expect_true(all(c(5L, 6L) %in% sel$playing))  # die mit 0 Spielen müssen rein
})

test_that("generate_candidate erzeugt vollständige, disjunkte Felder", {
  cand <- generate_candidate(players = 1:8, better_half = 1:4, worse_half = 5:8, num_fields = 2L)
  ids <- unlist(lapply(cand, function(p) c(p$team1, p$team2)))
  expect_length(ids, 8L)
  expect_length(unique(ids), 8L)
})

test_that("score_draw bestraft Partner-Wiederholung am höchsten", {
  hist <- list(partner = list("1" = 2L), prev = list(), team = list(), opp = list())
  rk <- data.frame(player_id = 1:8, rank = 1:8)
  repeat_partner <- list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L)))
  fresh        <- list(list(field = 1L, team1 = c(1L, 5L), team2 = c(3L, 4L)))
  expect_gt(score_draw(repeat_partner, hist, rk)$penalty,
            score_draw(fresh, hist, rk)$penalty)
})

test_that("generate_round_draw respektiert n_fields-Override (weniger Felder diese Runde)", {
  s <- new_tournament_state()
  for (i in 1:8) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")   # Einstellung: 2 Felder
  d <- generate_round_draw(s, 2L, seed = 1L, n_fields = 1L)
  expect_equal(length(d$pairings), 1L)
  expect_equal(length(d$byes), 4L)                       # 8 Spieler, 1 Feld -> 4 spielen, 4 aus
})

test_that("generate_round_draw ist deterministisch je Seed und füllt alle Felder", {
  s <- new_tournament_state()
  for (i in 1:8) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  d1 <- generate_round_draw(s, round = 2L, seed = 42L)
  d2 <- generate_round_draw(s, round = 2L, seed = 42L)
  expect_identical(d1$pairings, d2$pairings)
  ids <- unlist(lapply(d1$pairings, function(p) c(p$team1, p$team2)))
  expect_length(unique(ids), 8L)
})

test_that("score_draw: 1 Team-Wiederholung (Prio 4) wiegt schwerer als viele Einzelgegner (Prio 5)", {
  rk <- data.frame(player_id = 1:16, rank = 1:16)
  pairs <- list(
    list(field = 1L, team1 = c(1L, 9L),  team2 = c(2L, 10L)),
    list(field = 2L, team1 = c(3L, 11L), team2 = c(4L, 12L)),
    list(field = 3L, team1 = c(5L, 13L), team2 = c(6L, 14L)),
    list(field = 4L, team1 = c(7L, 15L), team2 = c(8L, 16L))
  )
  # A: genau eine Team-Wiederholung, sonst sauber
  histA <- list(partner = list(), prev = list(), team = list("1" = "2|10"), opp = list())
  # B: viele Einzelgegner-Wiederholungen (4 pro Feld x 4 Felder = 16), keine Team-Wiederholung
  oppB <- list()
  for (p in pairs) for (a in p$team1) oppB[[as.character(a)]] <- p$team2
  histB <- list(partner = list(), prev = list(), team = list(), opp = oppB)
  expect_gt(score_draw(pairs, histA, rk)$penalty, score_draw(pairs, histB, rk)$penalty)
})

test_that("18 Spieler / 4 Felder => 16 spielen, 2 setzen aus, alle verschieden", {
  s <- new_tournament_state()
  for (i in 1:18) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 7L, 4L, "best_of_3_11")
  d <- generate_round_draw(s, round = 1L, seed = 7L)
  ids <- unlist(lapply(d$pairings, function(p) c(p$team1, p$team2)))
  expect_length(ids, 16L)
  expect_length(unique(ids), 16L)
  expect_length(d$byes, 2L)
})

test_that("über mehrere Runden bleibt jeder Spieler genau einmal pro Runde", {
  s <- new_tournament_state()
  for (i in 1:8) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  for (rnd in 1:3) {
    d <- generate_round_draw(s, round = rnd, seed = 100L + rnd)
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) {
      s <- ts_save_result(s, gid, c(11L, 11L), c(5L, 7L))
    }
    s <- ts_lock_round(s, rnd)
    ids <- unlist(lapply(d$pairings, function(p) c(p$team1, p$team2)))
    expect_length(unique(ids), length(ids))
  }
  succeed()
})

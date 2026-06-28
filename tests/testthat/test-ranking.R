source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")

# Helper: ein abgeschlossenes Spiel mit Satzergebnissen bauen
mk_game <- function(gid, rnd, fld, a, b, c, d, t1s, t2s) {
  g <- empty_games_df()[1, ]
  g$game_id <- gid; g$round <- rnd; g$field <- fld
  g$t1_p1 <- a; g$t1_p2 <- b; g$t2_p1 <- c; g$t2_p2 <- d
  g$t1_set1 <- t1s[1]; g$t2_set1 <- t2s[1]
  g$t1_set2 <- if (length(t1s) >= 2) t1s[2] else NA_integer_
  g$t2_set2 <- if (length(t2s) >= 2) t2s[2] else NA_integer_
  g$t1_set3 <- if (length(t1s) >= 3) t1s[3] else NA_integer_
  g$t2_set3 <- if (length(t2s) >= 3) t2s[3] else NA_integer_
  sw <- sets_won_from_scores(t1s, t2s)
  g$t1_points <- sw[1]; g$t2_points <- sw[2]
  g$locked <- TRUE
  g
}

test_that("calculate_player_stats: Sätze und echte Punkte aus den Satzspalten", {
  g <- mk_game(1L, 1L, 1L, 1L, 2L, 3L, 4L, c(11L, 9L, 11L), c(7L, 11L, 8L))  # t1 gewinnt 2:1
  s <- calculate_player_stats(g, 1:4)
  p1 <- s[s$player_id == 1L, ]
  expect_equal(p1$sets_won, 2L)
  expect_equal(p1$sets_lost, 1L)
  expect_equal(p1$match_wins, 1L)
  expect_equal(p1$match_losses, 0L)
  expect_equal(p1$rally_points_for, 31L)       # 11+9+11
  expect_equal(p1$rally_points_against, 26L)    # 7+11+8
  expect_equal(p1$rally_point_diff, 5L)
  p3 <- s[s$player_id == 3L, ]
  expect_equal(p3$sets_won, 1L)
  expect_equal(p3$rally_point_diff, -5L)
})

test_that("calculate_player_stats ohne Ergebnisse: Nullzeilen, korrekte Spalten", {
  s <- calculate_player_stats(empty_games_df(), c(1L, 2L))
  expect_equal(nrow(s), 2L)
  expect_true(all(s$sets_won == 0L))
  expect_true(all(c("match_wins", "sets_won", "rally_point_diff") %in% names(s)))
})

test_that("create_ranking: Primärsortierung nach gewonnenen Sätzen", {
  g <- mk_game(1L, 1L, 1L, 1L, 2L, 3L, 4L, c(11L, 11L), c(5L, 7L))  # p1,p2 gewinnen 2:0
  r <- create_ranking(g, 1:4)
  expect_equal(r$sets_won[r$player_id == 1L], 2L)
  expect_equal(r$sets_won[r$player_id == 3L], 0L)
  expect_lt(r$rank[r$player_id == 1L], r$rank[r$player_id == 3L])
})

test_that("create_ranking: tiebreaker_order steuert Differenz vs. direkten Vergleich", {
  g <- rbind(
    mk_game(1L, 1L, 1L, 1L, 3L, 2L, 4L, c(11L, 11L), c(9L, 9L)),  # p1&p3 schlagen p2&p4 knapp
    mk_game(2L, 1L, 2L, 2L, 4L, 5L, 6L, c(11L, 11L), c(2L, 2L))   # p2&p4 schlagen p5&p6 hoch
  )
  ids <- 1:6
  r_diff <- create_ranking(g, ids, tiebreaker_order = "diff_first")
  r_dir  <- create_ranking(g, ids, tiebreaker_order = "direct_first")
  # p1 und p2 beide sets_won == 2
  expect_equal(r_diff$sets_won[r_diff$player_id == 1L], 2L)
  expect_equal(r_diff$sets_won[r_diff$player_id == 2L], 2L)
  # diff_first: p2 hat höhere Punktedifferenz (+14 vs +4) -> vor p1
  expect_lt(r_diff$rank[r_diff$player_id == 2L], r_diff$rank[r_diff$player_id == 1L])
  # direct_first: p1 hat p2 im direkten Duell geschlagen -> vor p2
  expect_lt(r_dir$rank[r_dir$player_id == 1L], r_dir$rank[r_dir$player_id == 2L])
})

test_that("create_ranking validiert tiebreaker_order", {
  expect_error(create_ranking(empty_games_df(), 1:2, tiebreaker_order = "foo"))
})

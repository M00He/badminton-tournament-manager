source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
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

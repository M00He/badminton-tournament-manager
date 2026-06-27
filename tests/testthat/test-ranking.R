source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")

make_games <- function() {
  g <- empty_games_df()
  g <- rbind(g, transform(empty_games_df()[1, ],
    game_id = 1L, round = 1L, field = 1L,
    t1_p1 = 1L, t1_p2 = 2L, t2_p1 = 3L, t2_p2 = 4L,
    t1_points = 2L, t2_points = 0L, locked = TRUE))
  g
}

test_that("create_ranking zählt Siege/Niederlagen pro player_id", {
  r <- create_ranking(make_games(), c(1L, 2L, 3L, 4L))
  expect_equal(r$wins[r$player_id == 1L], 1L)
  expect_equal(r$losses[r$player_id == 3L], 1L)
  expect_equal(r$rank[1], 1L)
})

test_that("create_ranking ohne Ergebnisse liefert Nullzeilen sauber", {
  r <- create_ranking(empty_games_df(), c(1L, 2L))
  expect_equal(nrow(r), 2L)
  expect_true(all(r$games_played == 0L))
})

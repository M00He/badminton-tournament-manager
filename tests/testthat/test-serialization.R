source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")

test_that("state_to_json -> state_from_json ist verlustfrei (Round-Trip)", {
  s <- new_tournament_state(name = "RT", created_at = "2026-06-27T10:00:00")
  for (i in 1:4) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L, 11L), c(5L, 7L))
  back <- state_from_json(state_to_json(s))
  expect_equal(back$tournament_name, s$tournament_name)
  expect_equal(back$current_round, s$current_round)
  expect_equal(back$players, s$players)
  expect_equal(back$games$t1_points, s$games$t1_points)
  expect_equal(back$games$t1_set2, s$games$t1_set2)   # gespielter Satz bleibt erhalten
  expect_true(is.na(back$games$t1_set3[1]))           # ungespielter Satz bleibt NA
  expect_true(is.na(back$games$t2_set3[1]))
})

test_that("migrate_state hebt alte schema_version an", {
  raw <- list(schema_version = 1L, tournament_name = "Alt",
              settings = list(num_rounds = 5L, num_fields = 4L, game_system = "best_of_3_11"),
              status = "running", current_round = 1L,
              players = empty_players_df(), games = empty_games_df())
  m <- migrate_state(raw)
  expect_equal(m$schema_version, 2L)
})

test_that("migrate_state ergänzt fehlendes tiebreaker_order mit Default", {
  raw <- list(schema_version = 2L, tournament_name = "Alt",
              settings = list(num_rounds = 5L, num_fields = 4L, game_system = "best_of_3_11"),
              status = "running", current_round = 1L,
              players = empty_players_df(), games = empty_games_df())
  m <- migrate_state(raw)
  expect_equal(m$settings$tiebreaker_order, "diff_first")
})

source("../../functions/tournament_state.R", encoding = "UTF-8")

test_that("new_tournament_state hat leere, korrekt typisierte Struktur", {
  s <- new_tournament_state(name = "Test", created_at = "2026-06-27T10:00:00")
  expect_equal(s$schema_version, 2L)
  expect_equal(s$tournament_name, "Test")
  expect_equal(s$status, "setup")
  expect_equal(s$current_round, 1L)
  expect_equal(nrow(s$players), 0L)
  expect_equal(nrow(s$games), 0L)
  expect_true(all(c("player_id", "name", "gender", "active") %in% names(s$players)))
  expect_true(all(c("game_id", "round", "field",
                    "t1_p1", "t1_p2", "t2_p1", "t2_p2",
                    "t1_points", "t2_points", "locked") %in% names(s$games)))
})

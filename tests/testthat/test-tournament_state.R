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

test_that("ts_add_player vergibt stabile, aufsteigende IDs und verhindert Duplikate", {
  s <- new_tournament_state()
  s <- ts_add_player(s, "Anna", "w")
  s <- ts_add_player(s, "Ben", "m")
  expect_equal(s$players$player_id, c(1L, 2L))
  expect_equal(s$players$name, c("Anna", "Ben"))
  expect_true(all(s$players$active))
  expect_error(ts_add_player(s, "Anna", "w"), "existiert bereits")
})

test_that("ts_rename_player ändert Name/Geschlecht ohne ID-Wechsel", {
  s <- ts_add_player(new_tournament_state(), "Anna", "w")
  s <- ts_rename_player(s, 1L, "Anna B.", "w")
  expect_equal(s$players$player_id, 1L)
  expect_equal(s$players$name, "Anna B.")
})

test_that("ts_set_player_active schaltet aktiv/inaktiv; ts_active_players filtert", {
  s <- ts_add_player(ts_add_player(new_tournament_state(), "Anna", "w"), "Ben", "m")
  s <- ts_set_player_active(s, 2L, FALSE)
  expect_equal(ts_active_players(s)$player_id, 1L)
})

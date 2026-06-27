source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")

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
  expect_equal(nrow(s$players), 1L)
})

test_that("ts_set_player_active schaltet aktiv/inaktiv; ts_active_players filtert", {
  s <- ts_add_player(ts_add_player(new_tournament_state(), "Anna", "w"), "Ben", "m")
  s <- ts_set_player_active(s, 2L, FALSE)
  expect_equal(ts_active_players(s)$player_id, 1L)
})

# ---- Task 3: Turnierstart & Spiel-Mutationen ----

make_started <- function(n = 8) {
  s <- new_tournament_state()
  for (i in seq_len(n)) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  ts_start_tournament(s, num_rounds = 5L, num_fields = 2L, game_system = "best_of_3_11")
}

test_that("ts_start_tournament setzt Status und Einstellungen", {
  s <- make_started()
  expect_equal(s$status, "running")
  expect_equal(s$settings$num_fields, 2L)
  expect_equal(s$current_round, 1L)
})

test_that("ts_set_round_games schreibt Felder mit NA-Ergebnis, nicht gesperrt", {
  s <- make_started()
  pairings <- list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L)))
  s <- ts_set_round_games(s, 1L, pairings)
  g <- s$games[s$games$round == 1L & s$games$field == 1L, ]
  expect_equal(nrow(g), 1L)
  expect_equal(c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2), c(1L, 2L, 3L, 4L))
  expect_true(is.na(g$t1_points))
  expect_false(g$locked)
})

test_that("ts_save_result berechnet gewonnene Sätze bei best_of_3", {
  s <- make_started()
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  gid <- s$games$game_id[1]
  s <- ts_save_result(s, gid, t1_sets = c(11L, 8L, 11L), t2_sets = c(7L, 11L, 9L))
  g <- s$games[s$games$game_id == gid, ]
  expect_equal(g$t1_points, 2L)   # 2 Sätze gewonnen
  expect_equal(g$t2_points, 1L)
})

test_that("ts_advance_round nur bei komplett gesperrter, vollständiger Runde", {
  s <- make_started()
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  expect_error(ts_advance_round(s), "nicht abgeschlossen")
  gid <- s$games$game_id[1]
  s <- ts_save_result(s, gid, c(11L, 11L, NA), c(5L, 7L, NA))
  s <- ts_lock_round(s, 1L)
  s <- ts_advance_round(s)
  expect_equal(s$current_round, 2L)
})

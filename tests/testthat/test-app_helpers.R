source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")

test_that("safe_filename säubert Strings", {
  expect_equal(safe_filename("Vereins Turnier 2026!"), "Vereins_Turnier_2026")
  expect_equal(safe_filename(""), "turnier")
  expect_equal(safe_filename("a/b\\c"), "a_b_c")
})

test_that("backup_filename nutzt Name und Runde", {
  s <- new_tournament_state(name = "Sommer Cup"); s$current_round <- 3L
  expect_equal(backup_filename(s), "Sommer_Cup_runde3.json")
  expect_equal(backup_filename(new_tournament_state()), "turnier_runde1.json")
})

test_that("state_summary liefert Vorschau-Felder", {
  s <- new_tournament_state(name = "X")
  for (i in 1:4) s <- ts_add_player(s, paste("P", i), "m")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  summ <- state_summary(s)
  expect_equal(summ$name, "X")
  expect_equal(summ$n_players, 4L)
  expect_equal(summ$num_rounds, 5L)
  expect_equal(summ$status_label, "Läuft")
})

test_that("player_name liefert Namen oder Fragezeichen", {
  s <- ts_add_player(new_tournament_state(), "Anna", "w")
  expect_equal(player_name(s, 1L), "Anna")
  expect_equal(player_name(s, 99L), "?")
})

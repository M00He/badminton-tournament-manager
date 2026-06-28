source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")

mk_players <- function(n) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(n)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s
}

test_that("ts_start_tournament Plan-Modus speichert Felder-Folge und leitet Runden/Felder ab", {
  s <- mk_players(8)
  fs <- c(2L, 2L, 2L, 1L, 1L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  expect_equal(s$settings$schedule_mode, "plan")
  expect_equal(s$settings$plan_field_sequence, fs)
  expect_equal(s$settings$num_rounds, 5L)        # = length(fs), num_rounds-Arg ignoriert
  expect_equal(s$settings$num_fields, 2L)         # = max(fs)
  expect_equal(s$status, "running")
})

test_that("ts_start_tournament Plan-Modus ohne Felder-Folge ist ein Fehler", {
  s <- mk_players(8)
  expect_error(ts_start_tournament(s, 5L, 2L, "best_of_3_11", "diff_first",
                                   schedule_mode = "plan", plan_field_sequence = NULL))
})

test_that("ts_start_tournament Default bleibt round_by_round (rueckwaertskompatibel)", {
  s <- mk_players(8)
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  expect_equal(s$settings$schedule_mode, "round_by_round")
  expect_null(s$settings$plan_field_sequence)
  expect_equal(s$settings$num_rounds, 5L)
})

test_that("migrate_state: altes Backup ohne schedule_mode -> round_by_round; Plan-Folge -> integer", {
  raw_old <- list(schema_version = 2L, tournament_name = "X", created_at = "",
                  settings = list(num_rounds = 5, num_fields = 2, game_system = "best_of_3_11",
                                  tiebreaker_order = "diff_first"),
                  status = "running", current_round = 1, players = NULL, games = NULL)
  m <- migrate_state(raw_old)
  expect_equal(m$settings$schedule_mode, "round_by_round")

  raw_plan <- raw_old
  raw_plan$settings$schedule_mode <- "plan"
  raw_plan$settings$plan_field_sequence <- c(2, 2, 2, 1, 1)   # via JSON kommen Doubles
  m2 <- migrate_state(raw_plan)
  expect_type(m2$settings$plan_field_sequence, "integer")
  expect_equal(m2$settings$plan_field_sequence, c(2L,2L,2L,1L,1L))
})

test_that("Serialisierung erhaelt schedule_mode + plan_field_sequence (Round-Trip)", {
  s <- ts_start_tournament(mk_players(8), 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = c(2L,2L,2L,1L,1L))
  back <- state_from_json(state_to_json(s))
  expect_equal(back$settings$schedule_mode, "plan")
  expect_equal(back$settings$plan_field_sequence, c(2L,2L,2L,1L,1L))
})

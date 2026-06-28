source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_setup.R", encoding = "UTF-8")
library(shiny)

mk_players <- function(n) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(n)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s
}

test_that("module_setup: Plan-Modus startet Turnier mit Felder-Folge", {
  rv <- reactiveVal(mk_players(14))
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(schedule_mode = "plan", num_fields = 3,
                      game_system = "best_of_3_11", tiebreaker = "diff_first")
    session$setInputs(plan_rounds = "7")            # 14/3/7 -> G=6
    session$setInputs(start = 1)
    s <- rv()
    expect_equal(s$status, "running")
    expect_equal(s$settings$schedule_mode, "plan")
    expect_equal(length(s$settings$plan_field_sequence), 7L)
    expect_equal(s$settings$num_rounds, 7L)
  })
})

test_that("module_setup: Rundenweise-Modus startet wie bisher", {
  rv <- reactiveVal(mk_players(8))
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(schedule_mode = "round_by_round", num_rounds = 5, num_fields = 2,
                      game_system = "best_of_3_11", tiebreaker = "diff_first")
    session$setInputs(start = 1)
    s <- rv()
    expect_equal(s$status, "running")
    expect_equal(s$settings$schedule_mode, "round_by_round")
    expect_equal(s$settings$num_rounds, 5L)
    expect_null(s$settings$plan_field_sequence)
  })
})

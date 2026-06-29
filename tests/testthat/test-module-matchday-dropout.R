source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_matchday.R", encoding = "UTF-8")
library(shiny)

mk_mid_plan_md <- function(np = 12L, nf = 3L) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(np)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(np, nf, 6L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  for (rnd in 1:2) {
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
  }
  s   # current_round = 3, noch nicht ausgelost
}

test_that("module_matchday: Austritt im Plan-Modus -> inaktiv + Re-Plan (plan_dropout gesetzt)", {
  rv <- reactiveVal(mk_mid_plan_md())
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(leave_player = "12")
    session$setInputs(confirm_leave = 1)
    s <- rv()
    expect_false(12L %in% ts_active_players(s)$player_id)   # raus
    expect_true(isTRUE(s$settings$plan_dropout))            # re-geplant
    expect_equal(length(s$settings$plan_field_sequence), s$settings$num_rounds)
  })
})

test_that("module_matchday: Austritt blockiert, wenn aktuelle Runde schon ausgelost ist", {
  s <- mk_mid_plan_md()
  d <- plan_next_round_pairings(s, seed = 3, n_candidates = 60L)
  s <- ts_set_round_games(s, 3L, d$pairings)             # Runde 3 ist gelost
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(leave_player = "12")
    session$setInputs(confirm_leave = 1)
    expect_true(12L %in% ts_active_players(rv())$player_id) # NICHT entfernt
  })
})

test_that("module_matchday: Austritt im Rundenweise-Modus setzt nur inaktiv", {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11", schedule_mode = "round_by_round")
  s <- ts_set_round_games(s, 1L, list(list(field=1L,team1=c(1L,2L),team2=c(3L,4L)),
                                       list(field=2L,team1=c(5L,6L),team2=c(7L,8L))))
  for (gid in s$games$game_id) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
  s <- ts_lock_round(s, 1L); s <- ts_advance_round(s)   # current_round = 2, nicht gelost
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(leave_player = "8")
    session$setInputs(confirm_leave = 1)
    expect_false(8L %in% ts_active_players(rv())$player_id)
    expect_null(rv()$settings$plan_dropout)              # kein Re-Plan im Rundenweise
  })
})

source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")

# 12 Spieler, 3 Felder, Plan-Modus; 2 Runden gespielt; current_round = 3
mk_mid_plan <- function(np = 12L, nf = 3L) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(np)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(np, nf, 6L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  # Runde 1 + 2 aus dem Generator nehmen und mit Ergebnissen abschliessen
  for (rnd in 1:2) {
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
  }
  s   # current_round = 3
}

test_that(".dropout_play_info zaehlt Spiele + Partnerschaften nur unter Aktiven", {
  s <- mk_mid_plan()
  s <- ts_set_player_active(s, 12L, FALSE)            # Spieler 12 raus
  active <- ts_active_players(s)$player_id
  info <- .dropout_play_info(s, active)
  expect_equal(length(info$cur), length(active))
  expect_false("12" %in% names(info$cur))             # Aussteiger nicht enthalten
  # keine used-Partnerschaft enthaelt den Aussteiger
  expect_false(any(vapply(info$used, function(p) 12L %in% p, logical(1))))
})

test_that("replan_after_dropout: 12->11 liefert gueltigen gleich-viele-Spiele-Restplan", {
  s <- mk_mid_plan()
  s <- ts_set_player_active(s, 12L, FALSE)
  r <- replan_after_dropout(s, seed = 1L)
  expect_false(is.null(r))
  expect_true(r$num_rounds >= s$current_round)        # >= gespielte + >=1 Restrunde
  expect_equal(length(r$field_sequence), r$num_rounds)
  # die gespielten 2 Runden behalten ihre Felderzahl
  expect_equal(r$field_sequence[1:2], s$settings$plan_field_sequence[1:2])
})

test_that("replan_after_dropout: < 4 Aktive -> NULL", {
  s <- mk_mid_plan(np = 6L, nf = 1L)
  for (id in 3:6) s <- ts_set_player_active(s, id, FALSE) # nur 2 aktiv
  expect_null(replan_after_dropout(s, seed = 1L))
})

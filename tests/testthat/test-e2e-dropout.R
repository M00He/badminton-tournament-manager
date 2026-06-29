for (f in list.files("../../functions", pattern = "[.]R$", full.names = TRUE))
  source(f, encoding = "UTF-8")

test_that("E2E: Plan-Turnier mit Austritt mittendrin — Verbliebene gleich viele Spiele, keine Partner-Wdh.", {
  s <- new_tournament_state(name = "E2E-Drop")
  for (i in seq_len(12)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(12L, 3L, 6L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)

  drop_after <- 2L
  repeat {
    rnd <- s$current_round
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    expect_false(is.null(d), info = sprintf("Runde %d: kein Plan", rnd))
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
    if (s$status == "finished") break
    if (rnd == drop_after) {                         # nach Runde 2: Spieler 12 scheidet aus
      s <- ts_remove_player(s, 12L)
      r <- replan_after_dropout(s, seed = 1L)
      expect_false(is.null(r))
      s$settings$plan_field_sequence <- r$field_sequence
      s$settings$num_rounds <- r$num_rounds
      s$settings$plan_dropout <- TRUE
      s$plan_replan <- r$schedule
    }
  }

  active <- ts_active_players(s)$player_id
  full <- played_rounds_as_plan(s)
  # keine Partner-Wiederholung unter den Verbliebenen
  pk <- function(a, b) paste(sort(c(a, b)), collapse = "|")
  seen <- character(0); rep_found <- FALSE
  for (rd in full) for (gm in rd$games) for (tm in list(gm$team1, gm$team2))
    if (all(tm %in% active)) { key <- pk(tm[1], tm[2]); if (key %in% seen) rep_found <- TRUE; seen <- c(seen, key) }
  expect_false(rep_found)
  # alle Verbliebenen gleich viele Gesamt-Spiele
  cnt <- setNames(integer(length(active)), as.character(active))
  for (rd in full) for (gm in rd$games) for (p in c(gm$team1, gm$team2))
    if (p %in% active) cnt[as.character(p)] <- cnt[as.character(p)] + 1L
  expect_equal(length(unique(cnt)), 1L)
})

test_that("E2E: 8 Spieler / 2 Felder, Austritt nach R1 -> Restplan haelt bis zum Ende (war STUCK)", {
  s <- new_tournament_state(name = "E2E-8")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(8L, 2L, 5L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  # Runde 1
  d <- plan_next_round_pairings(s, seed = 1, n_candidates = 60L)
  s <- ts_set_round_games(s, 1L, d$pairings)
  for (gid in s$games$game_id[s$games$round == 1]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
  s <- ts_lock_round(s, 1L); s <- ts_advance_round(s)        # current_round = 2
  # Spieler 8 scheidet aus -> Restplan erzeugen + speichern
  s <- ts_remove_player(s, 8L)
  r <- replan_after_dropout(s, seed = 1L)
  expect_false(is.null(r))
  s$settings$plan_field_sequence <- r$field_sequence
  s$settings$num_rounds <- r$num_rounds
  s$settings$plan_dropout <- TRUE
  s$plan_replan <- r$schedule
  # bis zum Ende, dem gespeicherten Plan folgend (DARF nicht in eine Sackgasse laufen)
  repeat {
    rnd <- s$current_round
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 60L)
    expect_false(is.null(d), info = sprintf("Runde %d: Sackgasse!", rnd))
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
    s <- ts_lock_round(s, rnd); s <- ts_advance_round(s)
    if (s$status == "finished") break
  }
  active <- ts_active_players(s)$player_id                    # 7 Spieler
  full <- played_rounds_as_plan(s)
  cnt <- setNames(integer(length(active)), as.character(active))
  pk <- function(a, b) paste(sort(c(a, b)), collapse = "|"); seen <- character(0); rep_found <- FALSE
  for (rd in full) for (gm in rd$games) {
    for (p in c(gm$team1, gm$team2)) if (p %in% active) cnt[as.character(p)] <- cnt[as.character(p)] + 1L
    for (tm in list(gm$team1, gm$team2)) if (all(tm %in% active)) {
      key <- pk(tm[1], tm[2]); if (key %in% seen) rep_found <- TRUE; seen <- c(seen, key) }
  }
  expect_equal(length(unique(cnt)), 1L)   # alle 7 gleich viele Gesamt-Spiele
  expect_false(rep_found)                  # keine Partner-Wiederholung
  expect_false(8L %in% active)
})

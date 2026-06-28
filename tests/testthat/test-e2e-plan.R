for (f in list.files("../../functions", pattern = "[.]R$", full.names = TRUE))
  source(f, encoding = "UTF-8")

test_that("E2E: Plan-Modus 14 Spieler / 3 Felder / 7 Runden — gleiche Spiele, keine Partner-Wiederholung, mit Pausen", {
  s <- new_tournament_state(name = "E2E")
  for (i in seq_len(14)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(14L, 3L, 7L)             # rep(3,7), G=6, jeder genau 1 Pause
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)

  # Runde 1 manuell: 3 Felder (12 Spieler), 13 & 14 pausieren
  r1 <- list(list(field = 1L, team1 = c(1L,2L),  team2 = c(3L,4L)),
             list(field = 2L, team1 = c(5L,6L),  team2 = c(7L,8L)),
             list(field = 3L, team1 = c(9L,10L), team2 = c(11L,12L)))
  s <- ts_set_round_games(s, 1L, r1)

  # Bis Ende durchspielen: Ergebnisse eintragen, sperren, Plan-Runde erzeugen, übernehmen
  repeat {
    rnd <- s$current_round
    for (gid in s$games$game_id[s$games$round == rnd]) {
      s <- ts_save_result(s, gid, c(11L, 11L, NA), c(5L, 7L, NA))
    }
    s <- ts_lock_round(s, rnd)
    s <- ts_advance_round(s)
    if (s$status == "finished") break
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 80L)
    expect_false(is.null(d), info = sprintf("Runde %d: kein Plan", s$current_round))
    s <- ts_set_round_games(s, s$current_round, d$pairings)
  }

  # Gesamtplan aus state$games verifizieren
  full <- played_rounds_as_plan(s)
  expect_equal(length(full), 7L)
  v <- verify_schedule(full, 1:14)
  expect_true(v$ok, info = paste(v$errors, collapse = "; "))
  expect_equal(length(v$partner_repeats), 0L)         # keine Partner-Wiederholung
  expect_true(v$equal_games)
  G <- sum(4L * fs) %/% 14L                            # = 6
  expect_equal(unname(v$games_per_player["1"]), G)
  expect_true(all(v$byes_per_player == 7L - G))        # jeder genau (R - G) = 1 Pause
})

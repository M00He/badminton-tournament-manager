source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")

# Gültiges Ergebnis für ein System (Team 1 gewinnt)
valid_res <- function(system) {
  info <- get_game_system_info(system)
  if (info$is_best_of_3) list(c(11L, 11L), c(5L, 7L)) else list(info$min_points, info$min_points - 5L)
}

# Ein komplettes Turnier spielen: Runde 1 manuell (erste passende Spieler), ab Runde 2 automatisch.
play_full <- function(n_players, n_fields, n_rounds, system, tb = "diff_first") {
  s <- new_tournament_state(name = "Sim")
  for (i in seq_len(n_players)) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, as.integer(n_rounds), as.integer(n_fields), system, tb)
  vr <- valid_res(system); cap <- n_fields * 4
  for (rnd in seq_len(n_rounds)) {
    if (rnd == 1) {
      active <- ts_active_players(s)$player_id
      nfit <- (min(length(active), cap) %/% 4) * 4
      pl <- active[seq_len(nfit)]
      pairings <- lapply(seq_len(nfit %/% 4), function(f) {
        b <- (f - 1) * 4; list(field = f, team1 = pl[b + 1:2], team2 = pl[b + 3:4])
      })
      s <- ts_set_round_games(s, 1L, pairings)
    } else {
      d <- generate_round_draw(s, s$current_round, seed = 10L + rnd)
      s <- ts_set_round_games(s, s$current_round, d$pairings)
    }
    g <- s$games[s$games$round == s$current_round, ]
    for (gid in g$game_id) s <- ts_save_result(s, gid, vr[[1]], vr[[2]])
    s <- ts_lock_round(s, s$current_round)
    s <- ts_advance_round(s)
  }
  s
}

# Invarianten prüfen; gibt Vektor der Probleme zurück (leer = ok)
check_tournament <- function(s, tb) {
  p <- character(0)
  if (s$status != "finished") p <- c(p, "nicht finished")
  for (rnd in unique(s$games$round)) {
    g <- s$games[s$games$round == rnd, ]
    ids <- c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2)
    if (length(ids) != length(unique(ids))) p <- c(p, paste("Spieler doppelt in Runde", rnd))
  }
  ids <- ts_active_players(s)$player_id
  r <- create_ranking(s$games, ids, tb)
  if (nrow(r) != length(ids)) p <- c(p, "Ranglisten-Größe")
  if (nrow(r) > 0 && !setequal(r$rank, seq_len(nrow(r)))) p <- c(p, "Ränge fehlerhaft")
  back <- state_from_json(state_to_json(s))
  if (nrow(back$games) != nrow(s$games) || back$status != "finished") p <- c(p, "Backup-Round-Trip")
  p
}

test_that("voller Turnierdurchlauf über viele Konfigurationen (Aussetzer, Systeme, Tiebreaker)", {
  cfgs <- expand.grid(np = c(4, 5, 7, 10, 18), nf = c(1, 2, 4),
                      sys = c("best_of_3_11", "single_15"),
                      stringsAsFactors = FALSE)
  for (k in seq_len(nrow(cfgs))) {
    c1 <- cfgs[k, ]
    s <- play_full(c1$np, c1$nf, 5L, c1$sys)
    probs <- check_tournament(s, "diff_first")
    expect_true(length(probs) == 0,
                info = sprintf("np=%d nf=%d %s -> %s", c1$np, c1$nf, c1$sys, paste(probs, collapse = "; ")))
  }
})

test_that("4 Spieler über 7 Runden läuft durch (Partner-Wiederholung unvermeidbar)", {
  s <- play_full(4, 1, 7L, "best_of_3_11", tb = "direct_first")
  expect_equal(s$status, "finished")
  expect_length(check_tournament(s, "direct_first"), 0)
})

test_that("ohne Aussetzer spielt jeder jede Runde (gleiche Spielzahl)", {
  s <- play_full(8, 2, 5L, "best_of_3_11")   # 8 Spieler, 2 Felder = 8 Plätze, keine Aussetzer
  r <- create_ranking(s$games, ts_active_players(s)$player_id, "diff_first")
  expect_true(all(r$games_played == 5L))
})

test_that("Ergebnis-Korrektur dreht den Sieger", {
  s <- new_tournament_state()
  for (i in 1:4) s <- ts_add_player(s, paste("P", i), "m")
  s <- ts_start_tournament(s, 1L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L))))
  gid <- s$games$game_id[1]
  s <- ts_save_result(s, gid, c(11L, 11L), c(5L, 7L)); s <- ts_lock_round(s, 1L)
  expect_true(create_ranking(s$games, 1:4, "diff_first")$player_id[1] %in% c(1L, 2L))
  s <- ts_edit_result(s, gid, c(5L, 7L), c(11L, 11L))
  expect_true(create_ranking(s$games, 1:4, "diff_first")$player_id[1] %in% c(3L, 4L))
})

test_that("Spieler nach gespieltem Spiel entfernen: inaktiv, Historie bleibt, Rangliste ok", {
  s <- new_tournament_state()
  for (i in 1:6) s <- ts_add_player(s, paste("P", i), "m")
  s <- ts_start_tournament(s, 2L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L, 11L), c(5L, 7L)); s <- ts_lock_round(s, 1L)
  s <- ts_remove_player(s, 1L)
  expect_equal(nrow(s$players), 6L)
  expect_false(s$players$active[s$players$player_id == 1L])
  expect_silent(create_ranking(s$games, ts_active_players(s)$player_id, "diff_first"))
})

test_that("leere Kategorie liefert 0-Zeilen-Rangliste (kein Crash)", {
  s <- new_tournament_state()
  for (i in 1:4) s <- ts_add_player(s, paste("M", i), "m")
  s <- ts_start_tournament(s, 1L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L, 11L), c(5L, 7L))
  women <- s$players$player_id[s$players$gender == "w"]
  expect_equal(nrow(create_ranking(s$games, women, "diff_first")), 0L)
})

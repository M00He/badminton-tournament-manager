source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")

# 8 Spieler, 2 Felder, 5 Runden Plan-Modus; Runde 1 gespielt + Ergebnisse.
mk_started_plan <- function() {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(8L, 2L, 5L)            # rep(2,5), G=5 (8 Spieler, 2 Felder -> keine Pause)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  r1 <- list(
    list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L)),
    list(field = 2L, team1 = c(5L,6L), team2 = c(7L,8L)))
  s <- ts_set_round_games(s, 1L, r1)
  for (gid in s$games$game_id[s$games$round == 1]) {
    s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))  # Team1 gewinnt
  }
  s <- ts_lock_round(s, 1L)
  s <- ts_advance_round(s)                          # current_round = 2
  s
}

test_that("games_round_to_plan uebersetzt eine gespielte Runde ins Planner-Format", {
  s <- mk_started_plan()
  rd <- games_round_to_plan(s, 1L)
  expect_equal(rd$field_count, 2L)
  expect_equal(length(rd$games), 2L)
  expect_equal(rd$games[[1]]$team1, c(1L,2L))
  expect_equal(rd$byes, integer(0))               # 8 Spieler, 2 Felder -> keine Pause
})

test_that("played_rounds_as_plan liefert den gespielten Praefix", {
  s <- mk_started_plan()
  pp <- played_rounds_as_plan(s)
  expect_equal(length(pp), 1L)                     # nur Runde 1 gespielt
  expect_equal(pp[[1]]$games[[2]]$team1, c(5L,6L))
})

test_that("strength_from_ranking: Sieger sind staerker als Verlierer", {
  s <- mk_started_plan()
  st <- strength_from_ranking(s)
  expect_named(st)
  # Team1-Spieler (1,2,5,6 haben gewonnen) staerker als Team2-Spieler (3,4,7,8)
  expect_gt(mean(st[c("1","2","5","6")]), mean(st[c("3","4","7","8")]))
})

test_that("plan_next_round_pairings: gueltige Runde 2, keine Partner-Wiederholung ggue. Runde 1", {
  s <- mk_started_plan()
  d <- plan_next_round_pairings(s, seed = 1L, n_candidates = 50L)
  expect_false(is.null(d))
  expect_equal(length(d$pairings), 2L)             # field_sequence[2] = 2 Felder
  players2 <- unlist(lapply(d$pairings, function(p) c(p$team1, p$team2)))
  expect_equal(sort(players2), 1:8)                # alle 8 spielen (keine Pause in Runde 2)
  # H2 ueber die Runde-1/Runde-2-Grenze: keine Runde-1-Partnerschaft wiederholt sich
  pkey <- function(a, b) paste(sort(c(a, b)), collapse = "|")
  r1_pairs <- c(pkey(1,2), pkey(3,4), pkey(5,6), pkey(7,8))
  r2_pairs <- unlist(lapply(d$pairings, function(p) c(pkey(p$team1[1], p$team1[2]),
                                                      pkey(p$team2[1], p$team2[2]))))
  expect_length(intersect(r1_pairs, r2_pairs), 0L)
})

test_that("plan_next_round_pairings: voller Plan (Praefix + Vorschlag) ist H1/H2-konform", {
  s <- mk_started_plan()
  d <- plan_next_round_pairings(s, seed = 2L, n_candidates = 50L)
  # Baue Runde-2 ins Planner-Format und verifiziere den 2-Runden-Ausschnitt
  r2 <- list(field_count = 2L, games = d$pairings, byes = as.integer(d$byes))
  two <- c(played_rounds_as_plan(s), list(r2))
  v <- verify_schedule(two, 1:8)
  expect_equal(length(v$partner_repeats), 0L)      # keine Partner-Wiederholung
})

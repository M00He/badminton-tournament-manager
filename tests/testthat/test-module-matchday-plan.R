source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_matchday.R", encoding = "UTF-8")
library(shiny)

# 8 Spieler, 2 Felder, 5 Runden, Plan-Modus; Runde 1 gespielt+gesperrt, current_round=2
mk_plan_round2 <- function() {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(8L, 2L, 5L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  r1 <- list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L)),
             list(field = 2L, team1 = c(5L,6L), team2 = c(7L,8L)))
  s <- ts_set_round_games(s, 1L, r1)
  for (gid in s$games$game_id[s$games$round == 1]) s <- ts_save_result(s, gid, c(11L,11L,NA), c(5L,7L,NA))
  s <- ts_lock_round(s, 1L); s <- ts_advance_round(s)
  s
}

test_that("module_matchday Plan-Modus: Vorschlag fuer Runde 2 kommt aus dem Generator", {
  rv <- reactiveVal(mk_plan_round2())
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1)
    d <- preview_rv()
    expect_false(is.null(d))
    expect_equal(d$quality, "gleiche Spielzahl + keine Partner-Wiederholung (garantiert)")
    expect_equal(length(d$pairings), 2L)             # fixe Felderzahl fs[2] = 2
    session$setInputs(accept = 1)
    g <- rv()$games[rv()$games$round == 2, ]
    expect_equal(nrow(g), 2L)
    # keine Partner-Wiederholung ggue. Runde 1
    pkey <- function(a,b) paste(sort(c(a,b)), collapse="|")
    r1 <- c("1|2","3|4","5|6","7|8")
    r2 <- c(pkey(g$t1_p1[1],g$t1_p2[1]), pkey(g$t2_p1[1],g$t2_p2[1]),
            pkey(g$t1_p1[2],g$t1_p2[2]), pkey(g$t2_p1[2],g$t2_p2[2]))
    expect_length(intersect(r1, r2), 0L)
  })
})

test_that("module_matchday Plan-Modus: round_fields folgt der Felder-Folge (ignoriert Picker)", {
  rv <- reactiveVal(mk_plan_round2())
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(round_fields = 1)              # Picker-Versuch -> im Plan-Modus ignoriert
    expect_equal(round_fields(), 2L)                  # fs[2] = 2
  })
})

test_that("module_matchday Rundenweise-Modus bleibt unveraendert (Greedy-Auslosung)", {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11", schedule_mode = "round_by_round")
  s$current_round <- 2L
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1)
    expect_true(length(preview_rv()$pairings) > 0)
  })
})

source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_matchday.R", encoding = "UTF-8")
library(shiny)

mk_started <- function(n = 8, fields = 2) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(n)) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  ts_start_tournament(s, 5L, fields, "best_of_3_11")
}

test_that("module_matchday: Runde 1 manuell eintragen schreibt die Paarungen", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(
      m_f1_s1 = "1", m_f1_s2 = "2", m_f1_s3 = "3", m_f1_s4 = "4",
      m_f2_s1 = "5", m_f2_s2 = "6", m_f2_s3 = "7", m_f2_s4 = "8")
    session$setInputs(manual_accept = 1)
    g <- rv()$games[rv()$games$round == 1, ]
    expect_equal(nrow(g), 2L)                                   # 2 Felder
    expect_equal(sort(c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2)), 1:8)
    expect_true(all(is.na(g$t1_points)))                       # noch keine Ergebnisse
  })
})

test_that("module_matchday: Runde 1 manuell blockiert doppelten Spieler", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(
      m_f1_s1 = "1", m_f1_s2 = "1", m_f1_s3 = "3", m_f1_s4 = "4",  # Spieler 1 doppelt
      m_f2_s1 = "5", m_f2_s2 = "6", m_f2_s3 = "7", m_f2_s4 = "8")
    session$setInputs(manual_accept = 1)
    expect_equal(nrow(rv()$games), 0L)   # nichts geschrieben
  })
})

test_that("module_matchday: Auslosungsvorschlag wird bei Rundenwechsel verworfen", {
  s <- mk_started(8, 2); s$current_round <- 2L
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1)
    expect_false(is.null(preview_rv()))          # Vorschlag liegt vor
    s2 <- rv(); s2$current_round <- 3L; rv(s2)    # Runde wechselt (z. B. Neustart)
    session$flushReact()
    expect_null(preview_rv())                     # Vorschlag verworfen
  })
})

test_that("module_matchday: ab Runde 2 Vorschau erzeugen + übernehmen (kein Schreiben bei reroll)", {
  s <- mk_started(8, 2); s$current_round <- 2L
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1)
    expect_true(length(preview_rv()$pairings) > 0)
    session$setInputs(reroll = 1)
    expect_true(length(preview_rv()$pairings) > 0)
    expect_equal(nrow(rv()$games), 0L)   # reroll schreibt nicht
    session$setInputs(accept = 1)
    g <- rv()$games[rv()$games$round == 2, ]
    expect_equal(nrow(g), 2L)
  })
})

test_that("module_matchday: gültiges Ergebnis speichern setzt Sätze, Lock + Advance", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1); session$setInputs(accept = 1)
    gids <- rv()$games$game_id[rv()$games$round == 1]
    for (gid in gids) {
      args_list <- setNames(
        list(11, 11, NA, 5, 7, NA, 1),
        c(paste0("t1s1_", gid), paste0("t1s2_", gid), paste0("t1s3_", gid),
          paste0("t2s1_", gid), paste0("t2s2_", gid), paste0("t2s3_", gid),
          paste0("save_", gid))
      )
      do.call(session$setInputs, args_list)
      save_args <- list(save_game = gid)
      do.call(session$setInputs, save_args)
    }
    expect_true(all(!is.na(rv()$games$t1_points[rv()$games$round == 1])))
    session$setInputs(lock_round = 1)
    expect_true(all(rv()$games$locked[rv()$games$round == 1]))
    session$setInputs(next_round = 1)
    expect_equal(rv()$current_round, 2L)
  })
})

test_that("module_matchday: ungültiges Ergebnis wird blockiert", {
  rv <- reactiveVal(mk_started(4, 1))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(preview = 1); session$setInputs(accept = 1)
    gid <- rv()$games$game_id[1]
    # 11:11 in beiden Sätzen -> kein Gewinner -> ungültig
    args_list <- setNames(
      list(11, 11, NA, 11, 11, NA, 1),
      c(paste0("t1s1_", gid), paste0("t1s2_", gid), paste0("t1s3_", gid),
        paste0("t2s1_", gid), paste0("t2s2_", gid), paste0("t2s3_", gid),
        paste0("save_", gid))
    )
    do.call(session$setInputs, args_list)
    save_args <- list(save_game = gid)
    do.call(session$setInputs, save_args)
    expect_true(is.na(rv()$games$t1_points[rv()$games$game_id == gid]))  # nicht gespeichert
  })
})

test_that("module_matchday: weniger Felder diese Runde (round_fields) reduziert die Auslosung", {
  s <- mk_started(8, 2); s$current_round <- 2L
  rv <- reactiveVal(s)
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(round_fields = 1)
    session$setInputs(preview = 1)
    expect_equal(length(preview_rv()$pairings), 1L)
    session$setInputs(accept = 1)
    expect_equal(nrow(rv()$games[rv()$games$round == 2, ]), 1L)
  })
})

test_that("module_matchday: Spieler eines Feldes von Hand tauschen", {
  rv <- reactiveVal(mk_started(8, 2))
  testServer(module_matchday_server, args = list(state_rv = rv), {
    session$setInputs(m_f1_s1 = "1", m_f1_s2 = "2", m_f1_s3 = "3", m_f1_s4 = "4",
                      m_f2_s1 = "5", m_f2_s2 = "6", m_f2_s3 = "7", m_f2_s4 = "8")
    session$setInputs(manual_accept = 1)
    gid <- rv()$games$game_id[rv()$games$field == 1 & rv()$games$round == 1]
    do.call(session$setInputs, setNames(list("1", "3", "2", "4"),
      paste0(c("p_t1p1_", "p_t1p2_", "p_t2p1_", "p_t2p2_"), gid)))
    session$setInputs(save_game = gid)
    row <- rv()$games[rv()$games$game_id == gid, ]
    expect_equal(c(row$t1_p1, row$t1_p2, row$t2_p1, row$t2_p2), c(1L, 3L, 2L, 4L))
  })
})

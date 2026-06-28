source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_setup.R", encoding = "UTF-8")
library(shiny)

test_that("module_setup: Spieler hinzufügen schreibt in den State", {
  rv <- reactiveVal(new_tournament_state())
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(new_name = "Anna", new_gender = "w")
    session$setInputs(add = 1)
    expect_equal(nrow(rv()$players), 1L)
    expect_equal(rv()$players$name, "Anna")
  })
})

test_that("module_setup: Turnier starten setzt Status + Einstellungen", {
  rv <- reactiveVal(new_tournament_state())
  for (nm in c("A","B","C","D")) isolate(rv(ts_add_player(rv(), nm, "m")))
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(num_rounds = 6, num_fields = 2, game_system = "best_of_3_11",
                      tiebreaker = "direct_first")
    session$setInputs(start = 1)
    expect_equal(rv()$status, "running")
    expect_equal(rv()$settings$num_rounds, 6L)
    expect_equal(rv()$settings$tiebreaker_order, "direct_first")
  })
})

test_that("module_setup: Testspieler generieren legt Spieler 1..N mit stabilen IDs an", {
  rv <- reactiveVal(new_tournament_state())
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(gen_count = 15)
    session$setInputs(gen_players = 1)
    expect_equal(nrow(rv()$players), 15L)
    expect_equal(rv()$players$name[1], "Spieler 1")
    expect_equal(rv()$players$name[15], "Spieler 15")
    expect_equal(rv()$players$player_id, 1:15)
  })
})

test_that("module_setup: 'Turnier starten' bei laufendem Turnier erst nach Bestätigung", {
  s <- new_tournament_state()
  for (nm in c("A","B","C","D")) s <- ts_add_player(s, nm, "m")
  s <- ts_start_tournament(s, 5L, 1L, "best_of_3_11")   # läuft schon (5 Runden)
  rv <- reactiveVal(s)
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(num_rounds = 3, num_fields = 1, game_system = "best_of_3_11",
                      tiebreaker = "diff_first")
    session$setInputs(start = 1)               # zeigt nur den Bestätigungs-Dialog
    expect_equal(rv()$settings$num_rounds, 5L) # noch NICHT neu gestartet
    session$setInputs(confirm_start_over = 1)  # bestätigt
    expect_equal(rv()$settings$num_rounds, 3L) # jetzt neu gestartet
  })
})

source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_ranking.R", encoding = "UTF-8")
library(shiny)

# kleines laufendes Turnier mit einem Ergebnis
mk_state <- function() {
  s <- new_tournament_state(name = "T")
  for (nm in c("A","B","C","D")) s <- ts_add_player(s, nm, "m")
  s <- ts_start_tournament(s, 3L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L,11L), c(5L,7L))
  s
}

test_that("module_ranking: ranking_data liefert sortierte Tabelle", {
  rv <- reactiveVal(mk_state())
  testServer(module_ranking_server, args = list(state_rv = rv), {
    session$setInputs(category = "all")
    d <- ranking_data()
    expect_true(all(c("rank","player_id","sets_won","rally_point_diff") %in% names(d)))
    expect_equal(d$sets_won[d$player_id == 1L], 2L)
    expect_lt(d$rank[d$player_id == 1L], d$rank[d$player_id == 3L])
  })
})

test_that("module_ranking: Kategorie-Filter schränkt auf Geschlecht ein", {
  s <- ts_set_player_active(mk_state(), 1L, TRUE)
  rv <- reactiveVal(s)
  testServer(module_ranking_server, args = list(state_rv = rv), {
    session$setInputs(category = "w")
    expect_equal(nrow(ranking_data()), 0L)  # keine Frauen im Testdatensatz
  })
})

test_that("module_ranking: Ergebnis nachträglich korrigieren (auch in gesperrter Runde)", {
  s <- ts_lock_round(mk_state(), 1L)          # Runde gesperrt
  rv <- reactiveVal(s)
  testServer(module_ranking_server, args = list(state_rv = rv), {
    gid <- rv()$games$game_id[1]
    session$setInputs(edit_game = gid)        # öffnet (gedankliches) Modal, setzt edit_gid
    session$setInputs(edit_t1s1 = 5, edit_t1s2 = 7, edit_t1s3 = NA,
                      edit_t2s1 = 11, edit_t2s2 = 11, edit_t2s3 = NA)
    session$setInputs(confirm_edit_game = 1)
    g <- rv()$games[rv()$games$game_id == gid, ]
    expect_equal(g$t1_points, 0L)             # Ergebnis umgedreht
    expect_equal(g$t2_points, 2L)
  })
})

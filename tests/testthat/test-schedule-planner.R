source("../../functions/schedule_planner.R", encoding = "UTF-8")

# Hilfsfunktion: baut eine Runde im verbindlichen Format
mk_round <- function(field_games, byes) {
  games <- lapply(seq_along(field_games), function(k) {
    fg <- field_games[[k]]
    list(field = k, team1 = fg[[1]], team2 = fg[[2]])
  })
  list(field_count = length(field_games), games = games, byes = byes)
}

test_that("verify_schedule erkennt einen gueltigen 4-Spieler-Plan", {
  players <- 1:4
  # 3 Runden, 1 Feld, jeder spielt jede Runde (keine Pausen), Partner rotieren
  sched <- list(
    mk_round(list(list(c(1L,2L), c(3L,4L))), integer(0)),
    mk_round(list(list(c(1L,3L), c(2L,4L))), integer(0)),
    mk_round(list(list(c(1L,4L), c(2L,3L))), integer(0))
  )
  v <- verify_schedule(sched, players)
  expect_true(v$ok)
  expect_true(v$equal_games)
  expect_equal(length(v$partner_repeats), 0L)
  expect_equal(unname(v$games_per_player["1"]), 3L)
})

test_that("verify_schedule erkennt Partner-Wiederholung", {
  players <- 1:4
  sched <- list(
    mk_round(list(list(c(1L,2L), c(3L,4L))), integer(0)),
    mk_round(list(list(c(1L,2L), c(3L,4L))), integer(0))  # 1&2 erneut Partner
  )
  v <- verify_schedule(sched, players)
  expect_false(v$ok)
  expect_true("1|2" %in% v$partner_repeats)
})

test_that("verify_schedule erkennt ungleiche Spielzahl", {
  players <- 1:6
  # Runde mit 1 Feld: 1,2 vs 3,4 spielen; 5,6 Pause -> nach 1 Runde ungleich
  sched <- list(mk_round(list(list(c(1L,2L), c(3L,4L))), c(5L,6L)))
  v <- verify_schedule(sched, players)
  expect_false(v$equal_games)
})

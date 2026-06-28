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

test_that("verify_schedule erkennt doppelt belegten Spieler in einer Runde", {
  players <- 1:4
  sched <- list(mk_round(list(list(c(1L,2L), c(1L,3L))), c(4L)))  # Spieler 1 zweimal aktiv
  v <- verify_schedule(sched, players)
  expect_false(v$ok)
  expect_true(any(grepl("doppelt", v$errors)))
})

test_that("verify_schedule erkennt falsche Pausen-Zuweisung", {
  players <- 1:6
  # nur 1,2,3,4 spielen -> 5,6 muessten Pause haben; hier faelschlich nur 6 als Pause
  sched <- list(mk_round(list(list(c(1L,2L), c(3L,4L))), c(6L)))
  v <- verify_schedule(sched, players)
  expect_false(v$ok)
  expect_true(any(grepl("Pausen", v$errors)))
})

test_that("max_games_for: 14 Spieler, 3 Felder", {
  expect_equal(max_games_for(14L, 3L, 7L), 6L)    # 7 Runden -> 6 Spiele
  expect_equal(max_games_for(14L, 3L, 11L), 8L)   # 11 Runden -> 8 Spiele
})

test_that("field_sequence_for: Summe und Obergrenze stimmen", {
  fs <- field_sequence_for(14L, 3L, 11L)
  expect_equal(length(fs), 11L)
  expect_equal(sum(fs), 14L * 8L / 4L)            # = 28
  expect_true(all(fs >= 1L & fs <= 3L))
  expect_equal(max_games_for(14L, 3L, 11L), 8L)
})

test_that("field_sequence_for: 7 Runden 14/3 nutzt durchgehend 3 Felder", {
  fs <- field_sequence_for(14L, 3L, 7L)
  expect_equal(fs, rep(3L, 7L))                   # 14*6/4 = 21 = 7*3
})

test_that("plan_options enthaelt 7- und 11-Runden-Variante fuer 14/3", {
  opts <- plan_options(14L, 3L)
  rs <- vapply(opts, function(o) o$rounds, integer(1))
  gs <- vapply(opts, function(o) o$games, integer(1))
  expect_true(7L %in% rs)
  expect_equal(gs[rs == 7L], 6L)
  expect_true(11L %in% rs)
  expect_equal(gs[rs == 11L], 8L)
  # jede Option: byes = rounds - games
  for (o in opts) expect_equal(o$byes, o$rounds - o$games)
})

test_that("default_plan_rounds schlaegt eine spielbare Rundenzahl vor", {
  R <- default_plan_rounds(14L, 3L)
  g <- max_games_for(14L, 3L, R)
  expect_true(g >= 6L && g <= 8L)
})

test_that("max_games_for: ungerades G ist gueltig wenn P*G durch 4 teilbar", {
  # 8 Spieler, 2 Felder, 3 Runden -> G=3 (jeder 3 verschiedene Partner, keine Pause)
  expect_equal(max_games_for(8L, 2L, 3L), 3L)
})

test_that("max_games_for / field_sequence_for: Infeasible -> 0 bzw. NULL", {
  expect_equal(max_games_for(5L, 1L, 3L), 0L)     # P=5: kein G mit 5G durch 4 teilbar im Bereich
  expect_null(field_sequence_for(5L, 1L, 3L))
})

test_that("Schutz vor absteigendem seq.int / max_rounds < 2", {
  expect_equal(max_games_for(2L, 1L, 5L), 0L)     # P=2 -> Obergrenze < 2 -> 0
  expect_equal(length(plan_options(14L, 3L, max_rounds = 1L)), 0L)
})

test_that("circle_factorization: P-1 Runden, alle Paare genau einmal", {
  P <- 8L
  rounds <- circle_factorization(P)
  expect_equal(length(rounds), P - 1L)            # 7 Runden
  # jede Runde: P/2 disjunkte Paare, deckt 1..P
  for (rd in rounds) {
    expect_equal(length(rd), P %/% 2L)
    expect_setequal(unlist(rd), 1:P)
  }
  # jedes Paar genau einmal über alle Runden
  keys <- unlist(lapply(rounds, function(rd)
    vapply(rd, function(p) paste(sort(p), collapse = "|"), character(1))))
  expect_equal(length(keys), length(unique(keys)))
  expect_equal(length(unique(keys)), choose(P, 2))  # alle C(P,2) Paare
})

test_that("circle_factorization: P=2 Grenzfall", {
  rounds <- circle_factorization(2L)
  expect_equal(length(rounds), 1L)
  expect_equal(rounds[[1]][[1]], c(2L, 1L))
})

test_that("circle_factorization: ungueltige Eingaben werden abgelehnt", {
  expect_error(circle_factorization(3L))   # ungerades P
  expect_error(circle_factorization(0L))   # P < 2
  expect_error(circle_factorization(1L))   # P < 2, ungerade
})

test_that("generate_schedule: 14/3, 7 Runden ist valide", {
  players <- 1:14
  fs <- field_sequence_for(14L, 3L, 7L)
  sched <- generate_schedule(players, fs, seed = 1L)
  expect_false(is.null(sched))
  v <- verify_schedule(sched, players)
  expect_true(v$ok)
  expect_equal(unname(v$games_per_player[1]), 6L)
  expect_equal(unname(v$byes_per_player[1]), 1L)
})

test_that("generate_schedule: Property-Sweep ueber mehrere Konfigurationen", {
  configs <- list(c(P=8, F=2, R=5), c(P=14, F=3, R=7), c(P=14, F=3, R=11),
                  c(P=12, F=3, R=6), c(P=16, F=4, R=7), c(P=18, F=4, R=9))
  for (cf in configs) {
    P <- cf["P"]; Fm <- cf["F"]; R <- cf["R"]
    fs <- field_sequence_for(P, Fm, R)
    expect_false(is.null(fs), info = sprintf("infeasible %d/%d/%d", P, Fm, R))
    for (sd in 1:3) {
      sched <- generate_schedule(seq_len(P), fs, seed = sd)
      expect_false(is.null(sched), info = sprintf("NULL %d/%d/%d seed %d", P, Fm, R, sd))
      v <- verify_schedule(sched, seq_len(P))
      expect_true(v$ok, info = sprintf("invalid %d/%d/%d seed %d: %s",
                                       P, Fm, R, sd, paste(v$errors, collapse = "; ")))
    }
  }
})

test_that("generate_schedule: Saettigung 8 Spieler/2 Felder/7 Runden (G=P-1)", {
  players <- 1:8                                   # 4F=8=P, keine Pausen, G=7=P-1
  fs <- field_sequence_for(8L, 2L, 7L)
  expect_equal(fs, rep(2L, 7L))
  sched <- generate_schedule(players, fs, seed = 1L)
  expect_false(is.null(sched))
  v <- verify_schedule(sched, players)
  expect_true(v$ok)
  expect_equal(unname(v$games_per_player[1]), 7L)
})

test_that("generate_schedule respektiert fixierte Runde 1", {
  players <- 1:14
  fs <- field_sequence_for(14L, 3L, 7L)            # 7x 3 Felder
  # manuelle Runde 1: 3 Felder, Paarungen frei gewählt, 2 Pausen (13,14)
  r1 <- list(field_count = 3L, byes = c(13L, 14L), games = list(
    list(field = 1L, team1 = c(1L, 2L),  team2 = c(3L, 4L)),
    list(field = 2L, team1 = c(5L, 6L),  team2 = c(7L, 8L)),
    list(field = 3L, team1 = c(9L, 10L), team2 = c(11L, 12L))))
  sched <- generate_schedule(players, fs, locked_rounds = list(r1), seed = 2L)
  expect_false(is.null(sched))
  # Runde 1 unveraendert
  expect_equal(sched[[1]]$games[[1]]$team1, c(1L, 2L))
  expect_equal(sort(sched[[1]]$byes), c(13L, 14L))
  v <- verify_schedule(sched, players)
  expect_true(v$ok)
  # die in R1 gesetzten Partner duerfen nicht erneut auftauchen (H2 ueber gesamten Plan)
  expect_false("1|2" %in% v$partner_repeats)
})

# generate_schedule VALIDIERT gelockte Runden NICHT (das macht die UI in Plan B).
# Es liefert NULL nur, wenn der gelockte Praefix keine gleiche-Spiele-Completion mehr zulaesst.
test_that("generate_schedule: gestrandete Spieler -> keine Completion -> NULL", {
  players <- 1:6
  fs <- field_sequence_for(6L, 1L, 3L)             # 3 Runden, 1 Feld, G=2, je 1 Pause
  expect_equal(fs, rep(1L, 3L))
  # Runden 1+2 lassen 1 und 2 pausieren -> sie haben 0 Spiele, koennen in 1 Restrunde
  # nicht auf G=2 kommen -> keine gueltige Completion.
  r1 <- list(field_count = 1L, byes = c(1L, 2L), games = list(
    list(field = 1L, team1 = c(3L, 4L), team2 = c(5L, 6L))))
  r2 <- list(field_count = 1L, byes = c(1L, 2L), games = list(
    list(field = 1L, team1 = c(3L, 5L), team2 = c(4L, 6L))))  # intern gueltig (keine Repeats)
  sched <- generate_schedule(players, fs, locked_rounds = list(r1, r2),
                             seed = 1L, max_restarts = 200L)
  expect_null(sched)
})

source("../../functions/schedule_planner.R", encoding = "UTF-8")

# zaehlt Spiele je Spieler in einem Plan
.count_games <- function(schedule, players) {
  cnt <- setNames(integer(length(players)), as.character(players))
  for (rd in schedule) for (gm in rd$games) {
    for (p in c(gm$team1, gm$team2)) cnt[as.character(p)] <- cnt[as.character(p)] + 1L
  }
  cnt
}
.has_pair <- function(schedule, a, b) {
  key <- paste(sort(c(a, b)), collapse = "|")
  any(vapply(schedule, function(rd) any(vapply(rd$games, function(gm)
    key %in% c(paste(sort(gm$team1), collapse = "|"), paste(sort(gm$team2), collapse = "|")),
    logical(1))), logical(1)))
}

test_that("generate_schedule: init_games bringt alle auf gleiche GESAMT-Spielzahl", {
  players <- 1:8
  init <- setNames(rep(2L, 8), as.character(1:8))        # jeder hat schon 2 Spiele
  fs <- c(2L, 2L, 2L)                                     # 3 Runden, alle spielen -> +3
  sched <- generate_schedule(players, fs, init_games = init, seed = 1L)
  expect_false(is.null(sched))
  added <- .count_games(sched, players)
  expect_true(all(added + 2L == 5L))                     # 2 + 3 = 5 fuer alle
})

test_that("generate_schedule: forbidden_pairs werden nie als Team erzeugt", {
  players <- 1:8
  init <- setNames(rep(2L, 8), as.character(1:8))
  fs <- c(2L, 2L, 2L)
  sched <- generate_schedule(players, fs, init_games = init,
                             forbidden_pairs = list(c(1L, 2L), c(3L, 4L)), seed = 3L)
  expect_false(is.null(sched))
  expect_false(.has_pair(sched, 1L, 2L))
  expect_false(.has_pair(sched, 3L, 4L))
  v <- verify_schedule(sched, players)                   # innerhalb des Plans keine Wdh.
  expect_equal(length(v$partner_repeats), 0L)
})

test_that("generate_schedule: ungleiche init werden ausgeglichen (Zurueckliegende spielen mehr)", {
  players <- 1:8
  init <- setNames(c(3L,3L,3L,3L,2L,2L,2L,2L), as.character(1:8))  # 4 bei 3, 4 bei 2
  fs <- c(2L, 1L)                                          # Sf=3 -> 4*3=12; (20+12)/8 ... pruefen wir via G
  # G = (sum(init)=20 + 4*sum(fs)=12)/8 = 32/8 = 4
  sched <- generate_schedule(players, fs, init_games = init, seed = 5L)
  expect_false(is.null(sched))
  added <- .count_games(sched, players)
  total <- added + init[as.character(players)]
  expect_true(all(total == 4L))
})

test_that("generate_schedule ohne init/forbidden ist unveraendert (Normalfall valide)", {
  players <- 1:8
  fs <- field_sequence_for(8L, 2L, 5L)
  sched <- generate_schedule(players, fs, seed = 1L)
  v <- verify_schedule(sched, players)
  expect_true(v$ok)
})

test_that("generate_schedule: forbidden_pairs auch OHNE init_games (Normalfall)", {
  players <- 1:8
  fs <- field_sequence_for(8L, 2L, 5L)             # G=5, keine Pausen
  sched <- generate_schedule(players, fs, forbidden_pairs = list(c(1L, 2L)), seed = 2L)
  expect_false(is.null(sched))
  expect_false(.has_pair(sched, 1L, 2L))
  v <- verify_schedule(sched, players)
  expect_true(v$ok)                                 # weiterhin gleiche Spiele + gleiche Pausen
})

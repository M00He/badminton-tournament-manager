# Voraus-Spielplan-Generator (Plan A: reiner Kern) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Eine reine, property-getestete R-Datei `functions/schedule_planner.R`, die einen kompletten Turnier-Spielplan erzeugt, der **gleiche Spiele für alle** und **keine Partner-Wiederholung** garantiert, mit variabler Felderzahl, fixierter Runde 1 und stark+schwach-Re-Optimierung.

**Architecture:** Reine Funktionen ohne Shiny-/State-Abhängigkeit. Ein unabhängiger Verifizierer (`verify_schedule`) prüft die Invarianten; ein randomisiert-konstruktiver Generator (`generate_schedule`) mit „muss-noch-spielen"-Regel + Neustarts baut Pläne; die Kreis-Methode (`circle_factorization`) ist die deterministische Sicherung für den Sättigungsfall; `plan_options` liefert die Feasibility-Leiter; `reoptimize_tail` wählt unter gültigen Restplänen den besten für die aktuelle Tabelle.

**Tech Stack:** Base R, `testthat`. Keine externen Solver, keine neuen Paket-Abhängigkeiten. Ausführung der Tests über PowerShell (`Rscript`).

## Global Constraints

- **Pure R:** nur base R (+ vorhandene `shiny`/`jsonlite`). KEINE externen Solver, KEINE neuen Paket-Abhängigkeiten.
- **Determinismus:** der Kern darf NICHT `Sys.time()`/`Date`/Echtzeit nutzen — Zufall ausschließlich über `set.seed(seed)`, damit Tests reproduzierbar sind.
- **Runden-Datenformat (verbindlich, kompatibel zu `ts_set_round_games`):** eine Runde ist `list(field_count = <int>, games = <list>, byes = <int vec player_ids>)`; jedes Element von `games` ist `list(field = <int>, team1 = c(<id>, <id>), team2 = c(<id>, <id>))`.
- **Harte Garantien (H1/H2):** alle Spieler gleich viele Spiele; kein Spieler zweimal mit demselben Partner. Gegner-Wiederholungen sind erlaubt.
- **Spieler-Identität:** Funktionen arbeiten auf einem Vektor echter `player_id`s (`players`), nicht auf 1..P. Intern darf auf 1..P gemappt werden.
- **Sprache:** deutsche Bezeichner/Strings konsistent mit der bestehenden App; Dateiname beschreibend (`schedule_planner.R`).
- **Tests:** neue Datei `tests/testthat/test-schedule-planner.R`, Muster wie bestehende Tests (`source("../../functions/...", encoding = "UTF-8")`).

---

### Task 1: Unabhängiger Verifizierer `verify_schedule`

Der Prüfer ist die Grundlage aller weiteren Property-Tests. Er ist bewusst unabhängig vom Generator geschrieben.

**Files:**
- Create: `functions/schedule_planner.R`
- Test: `tests/testthat/test-schedule-planner.R`

**Interfaces:**
- Consumes: nichts (erste Task).
- Produces: `verify_schedule(schedule, players) -> list(ok, games_per_player, byes_per_player, partner_repeats, equal_games, equal_byes, errors)`. `schedule` = Liste von Runden im Global-Constraints-Format; `players` = integer-Vektor der player_ids. `ok = TRUE` gdw. keine Fehler, keine Partner-Wiederholung, gleiche Spiele, gleiche Pausen.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-schedule-planner.R`:

```r
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
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run (PowerShell, im Repo-Root):
`Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: FEHLER — `could not find function "verify_schedule"`.

- [ ] **Step 3: `verify_schedule` implementieren**

Schreibe in `functions/schedule_planner.R`:

```r
# Spielplan-Generator: Feasibility, Konstruktion, Verifikation, Re-Optimierung.
# Reine Funktionen, kein Shiny/State, kein Echtzeit-Zufall (nur set.seed()).

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Prüft die harten Invarianten eines Plans (unabhängig vom Generator).
verify_schedule <- function(schedule, players) {
  P <- length(players)
  games_cnt <- setNames(integer(P), as.character(players))
  byes_cnt  <- setNames(integer(P), as.character(players))
  partner_seen <- list()
  errors <- character(0)

  for (r in seq_along(schedule)) {
    rd <- schedule[[r]]
    if (is.null(rd)) { errors <- c(errors, sprintf("Runde %d fehlt", r)); next }
    round_players <- integer(0)
    for (gm in rd$games) {
      round_players <- c(round_players, gm$team1, gm$team2)
      for (tm in list(gm$team1, gm$team2)) {
        key <- paste(sort(tm), collapse = "|")
        partner_seen[[key]] <- (partner_seen[[key]] %||% 0L) + 1L
      }
    }
    if (any(duplicated(round_players)))
      errors <- c(errors, sprintf("Runde %d: Spieler doppelt im Einsatz", r))
    gk <- as.character(round_players)
    games_cnt[gk] <- games_cnt[gk] + 1L
    if (length(rd$byes)) {
      bk <- as.character(rd$byes); byes_cnt[bk] <- byes_cnt[bk] + 1L
    }
    expected_bye <- setdiff(players, round_players)
    if (!setequal(rd$byes, expected_bye))
      errors <- c(errors, sprintf("Runde %d: Pausen stimmen nicht", r))
  }

  repeats <- names(partner_seen)[vapply(partner_seen, function(x) x > 1L, logical(1))]
  equal_games <- length(unique(games_cnt)) == 1L
  equal_byes  <- length(unique(byes_cnt)) == 1L
  ok <- length(errors) == 0L && length(repeats) == 0L && equal_games && equal_byes
  list(ok = ok, games_per_player = games_cnt, byes_per_player = byes_cnt,
       partner_repeats = repeats, equal_games = equal_games,
       equal_byes = equal_byes, errors = errors)
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: PASS (3 Tests grün).

- [ ] **Step 5: Commit**

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner.R
git commit -m "feat(planner): verify_schedule - unabhaengiger Invarianten-Pruefer"
```

---

### Task 2: Feasibility-Mathematik (`max_games_for`, `field_sequence_for`, `plan_options`, `default_plan_rounds`)

**Files:**
- Modify: `functions/schedule_planner.R`
- Test: `tests/testthat/test-schedule-planner.R`

**Interfaces:**
- Consumes: nichts.
- Produces:
  - `max_games_for(P, F_max, R) -> integer` — größtes gerades `G` mit `P*G` durch 4 teilbar, `G <= P-1`, `G <= R`, `R <= P*G/4 <= R*F_max`. `0L` wenn keins.
  - `field_sequence_for(P, F_max, R) -> integer[R]` — Felder-Folge (absteigend sortiert) mit `sum = P*G/4`, jedes in `1..F_max`; `NULL` wenn infeasible.
  - `plan_options(P, F_max, min_games = 4L, max_rounds = NULL) -> list` von `list(rounds, games, byes, field_sequence)`, aufsteigend nach `rounds`.
  - `default_plan_rounds(P, F_max) -> integer` — vorgeschlagene Rundenzahl (G möglichst in 6..8 bei wenigen Pausen).

- [ ] **Step 1: Failing test schreiben**

Ergänze in `tests/testthat/test-schedule-planner.R`:

```r
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
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: FEHLER — `could not find function "max_games_for"`.

- [ ] **Step 3: Implementieren**

Ergänze in `functions/schedule_planner.R`:

```r
# Größtes gerades G, das in R Runden bei P Spielern und max F_max Feldern aufgeht.
max_games_for <- function(P, F_max, R) {
  best <- 0L
  for (G in seq.int(2L, min(P - 1L, R), by = 1L)) {
    if ((P * G) %% 4L != 0L) next            # P*G/4 muss ganzzahlig sein
    S <- (P * G) %/% 4L                        # benötigte Feld-Summe
    if (S < R) next                            # jede Runde >= 1 Feld
    if (S > R * F_max) next                    # jede Runde <= F_max Felder
    best <- G
  }
  best
}

# Felder-Folge (absteigend) für R Runden; NULL falls infeasible.
field_sequence_for <- function(P, F_max, R) {
  G <- max_games_for(P, F_max, R)
  if (G == 0L) return(NULL)
  S <- (P * G) %/% 4L
  q <- S %/% R; rem <- S %% R
  fs <- c(rep(q + 1L, rem), rep(q, R - rem))    # Summe = S, jedes in {q, q+1}
  sort(fs, decreasing = TRUE)                    # mehr Felder zuerst
}

# Feasibility-Leiter: für jede sinnvolle Rundenzahl eine Option.
plan_options <- function(P, F_max, min_games = 4L, max_rounds = NULL) {
  if (is.null(max_rounds)) max_rounds <- P - 1L  # G <= P-1 ist die Obergrenze
  out <- list()
  for (R in seq.int(2L, max_rounds)) {
    G <- max_games_for(P, F_max, R)
    if (G < min_games) next
    fs <- field_sequence_for(P, F_max, R)
    if (is.null(fs)) next
    out[[length(out) + 1L]] <- list(rounds = R, games = G,
                                    byes = R - G, field_sequence = fs)
  }
  out
}

# Vorgeschlagene Rundenzahl: G möglichst in 6..8, bei Gleichstand wenige Pausen.
default_plan_rounds <- function(P, F_max) {
  opts <- plan_options(P, F_max)
  if (length(opts) == 0L) return(NA_integer_)
  score <- vapply(opts, function(o) {
    target <- if (o$games >= 6L && o$games <= 8L) 0L else min(abs(o$games - 6L), abs(o$games - 8L))
    target * 100L + o$byes                       # erst Ziel-Band, dann wenige Pausen
  }, integer(1))
  opts[[which.min(score)]]$rounds
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: PASS (alle bisherigen Tests grün).

- [ ] **Step 5: Commit**

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner.R
git commit -m "feat(planner): Feasibility-Mathematik (plan_options, field_sequence_for)"
```

---

### Task 3: Kreis-Methode `circle_factorization` (deterministische Sicherung)

**Files:**
- Modify: `functions/schedule_planner.R`
- Test: `tests/testthat/test-schedule-planner.R`

**Interfaces:**
- Consumes: nichts.
- Produces: `circle_factorization(P) -> list` von `P-1` Runden; jede Runde ist ein `list` von Paaren `c(a, b)` (eine perfekte Paarung von `1..P`, `P` gerade). Über alle Runden kommt jedes Paar `{i,j}` genau einmal vor.

- [ ] **Step 1: Failing test schreiben**

Ergänze:

```r
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
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: FEHLER — `could not find function "circle_factorization"`.

- [ ] **Step 3: Implementieren**

Ergänze in `functions/schedule_planner.R`:

```r
# 1-Faktorisierung von K_P (P gerade) per Kreis-/Round-Robin-Methode.
# Liefert P-1 perfekte Paarungen; jedes Paar {i,j} kommt genau einmal vor.
circle_factorization <- function(P) {
  stopifnot(P %% 2L == 0L, P >= 2L)
  fixed <- P
  ring <- seq_len(P - 1L)            # rotierende Spieler
  rounds <- vector("list", P - 1L)
  for (r in seq_len(P - 1L)) {
    pairs <- list()
    pairs[[1]] <- c(fixed, ring[1])  # fester Spieler gegen Kopf des Rings
    half <- (P - 2L) %/% 2L
    for (i in seq_len(half)) {
      a <- ring[1L + i]
      b <- ring[length(ring) - i + 1L]
      pairs[[length(pairs) + 1L]] <- c(a, b)
    }
    rounds[[r]] <- pairs
    ring <- c(ring[length(ring)], ring[-length(ring)])  # um 1 rotieren
  }
  rounds
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner.R
git commit -m "feat(planner): circle_factorization (deterministische 1-Faktorisierung)"
```

---

### Task 4: Generator `generate_schedule` (randomisiert-konstruktiv + Kreis-Fallback)

**Files:**
- Modify: `functions/schedule_planner.R`
- Test: `tests/testthat/test-schedule-planner.R`

**Interfaces:**
- Consumes: `field_sequence_for`, `max_games_for`, `circle_factorization`, `verify_schedule`.
- Produces: `generate_schedule(players, field_sequence, locked_rounds = NULL, seed = 1L, max_restarts = 2000L) -> list` von Runden im Global-Constraints-Format, oder `NULL` bei Fehlschlag. Garantiert (bei Nicht-NULL) H1/H2 + gleiche Pausen. `players` = integer player_ids; `field_sequence` = integer je Runde; `locked_rounds` = bereits fixierte Runden im selben Format (hier in Task 4 immer `NULL`).

- [ ] **Step 1: Failing test schreiben**

Ergänze:

```r
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
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: FEHLER — `could not find function "generate_schedule"`.

- [ ] **Step 3: Implementieren**

Ergänze in `functions/schedule_planner.R`:

```r
# Baut einen Plan aus einer Kreis-Faktorisierung (Sättigung: G = P-1, keine Pausen).
.schedule_from_circle <- function(players, field_sequence) {
  P <- length(players)
  R <- length(field_sequence)
  fac <- circle_factorization(P)                  # P-1 Runden Paarungen über 1..P
  rounds <- vector("list", R)
  for (r in seq_len(R)) {
    pairs <- fac[[r]]                              # P/2 Teams
    f <- field_sequence[r]
    if (length(pairs) != 2L * f) return(NULL)      # nur sauberer No-Bye-Fall
    games <- list()
    for (k in seq_len(f)) {
      t1 <- pairs[[2L * k - 1L]]; t2 <- pairs[[2L * k]]
      games[[k]] <- list(field = k,
                         team1 = players[t1], team2 = players[t2])
    }
    rounds[[r]] <- list(field_count = f, games = games, byes = integer(0))
  }
  rounds
}

# Randomisiert-konstruktiver Generator mit "muss-noch-spielen"-Regel + Neustarts.
generate_schedule <- function(players, field_sequence, locked_rounds = NULL,
                              seed = 1L, max_restarts = 2000L) {
  P <- length(players)
  R <- length(field_sequence)
  G <- (sum(4L * field_sequence)) %/% P
  idx <- seq_len(P)
  id_of <- players                                 # idx -> player_id
  to_idx <- function(id) match(id, id_of)
  n_locked <- if (is.null(locked_rounds)) 0L else length(locked_rounds)

  # Sättigungs-Sicherung: ohne Pausen und G = P-1 -> deterministisch via Kreis.
  no_byes <- all(field_sequence == P %/% 4L) && (P %% 4L == 0L)
  if (n_locked == 0L && G == P - 1L && no_byes) {
    sc <- .schedule_from_circle(players, field_sequence)
    if (!is.null(sc)) return(sc)
  }

  set.seed(seed)
  for (attempt in seq_len(max_restarts)) {
    partner_used <- matrix(FALSE, P, P)
    games_cnt <- integer(P); byes_cnt <- integer(P)
    rounds <- vector("list", R); ok <- TRUE

    if (n_locked > 0L) {
      for (r in seq_len(n_locked)) {
        lr <- locked_rounds[[r]]
        for (gm in lr$games) {
          a <- to_idx(gm$team1[1]); b <- to_idx(gm$team1[2])
          c <- to_idx(gm$team2[1]); d <- to_idx(gm$team2[2])
          partner_used[a, b] <- partner_used[b, a] <- TRUE
          partner_used[c, d] <- partner_used[d, c] <- TRUE
          games_cnt[c(a, b, c, d)] <- games_cnt[c(a, b, c, d)] + 1L
        }
        if (length(lr$byes)) {
          bi <- to_idx(lr$byes); byes_cnt[bi] <- byes_cnt[bi] + 1L
        }
        rounds[[r]] <- lr
      }
    }

    if (n_locked < R) for (r in (n_locked + 1L):R) {
      f <- field_sequence[r]; n_play <- 4L * f; n_bye <- P - n_play
      rem_rounds <- R - r + 1L
      need <- G - games_cnt
      must_play <- which(need >= rem_rounds)
      if (length(must_play) > n_play) { ok <- FALSE; break }
      cand_bye <- setdiff(idx, must_play)
      if (length(cand_bye) < n_bye) { ok <- FALSE; break }
      sitout <- cand_bye[order(byes_cnt[cand_bye], runif(length(cand_bye)))][seq_len(n_bye)]
      active <- sample(setdiff(idx, sitout))

      free <- active; teams <- list(); pair_ok <- TRUE
      while (length(free) >= 2L) {
        a <- free[1]; rest <- free[-1]
        compat <- rest[!partner_used[a, rest]]
        if (length(compat) == 0L) { pair_ok <- FALSE; break }
        b <- compat[sample.int(length(compat), 1L)]
        teams[[length(teams) + 1L]] <- c(a, b)
        free <- setdiff(free, c(a, b))
      }
      if (!pair_ok || length(teams) != 2L * f) { ok <- FALSE; break }

      games <- list()
      for (k in seq_len(f)) {
        t1 <- teams[[2L * k - 1L]]; t2 <- teams[[2L * k]]
        games[[k]] <- list(field = k, team1 = id_of[t1], team2 = id_of[t2])
        partner_used[t1[1], t1[2]] <- partner_used[t1[2], t1[1]] <- TRUE
        partner_used[t2[1], t2[2]] <- partner_used[t2[2], t2[1]] <- TRUE
      }
      games_cnt[active] <- games_cnt[active] + 1L
      byes_cnt[sitout] <- byes_cnt[sitout] + 1L
      rounds[[r]] <- list(field_count = f, games = games, byes = id_of[sitout])
    }

    if (ok && all(games_cnt == G) && all(byes_cnt == (R - G))) return(rounds)
  }
  NULL
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: PASS (Property-Sweep grün). Falls eine Konfiguration langsam ist (>2 s), genügt `max_restarts`; melden falls eine NULL liefert.

- [ ] **Step 5: Commit**

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner.R
git commit -m "feat(planner): generate_schedule (konstruktiv + muss-spielen-Regel + Kreis-Fallback)"
```

---

### Task 5: Fixierte Runde 1 (`locked_rounds`) konditionieren

**Files:**
- Modify: `functions/schedule_planner.R` (nur falls eine Korrektur nötig ist — `generate_schedule` unterstützt `locked_rounds` bereits)
- Test: `tests/testthat/test-schedule-planner.R`

**Interfaces:**
- Consumes: `generate_schedule(players, field_sequence, locked_rounds, ...)`, `verify_schedule`.
- Produces: keine neue Funktion; verifiziertes Verhalten von `locked_rounds` (Runde 1 bleibt erhalten, Gesamtplan valide; infeasible Runde 1 -> `NULL`).

- [ ] **Step 1: Failing test schreiben**

Ergänze:

```r
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
```

- [ ] **Step 2: Test laufen lassen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: Beide neuen Tests sollten direkt PASS sein (Funktion unterstützt `locked_rounds` schon). Falls einer FAILt, in Step 3 korrigieren.

- [ ] **Step 3: (Nur falls nötig) Korrektur**

Falls Step 2 zeigt, dass `locked_rounds` nicht korrekt vorgeladen wird (z. B. ein NULL trotz lösbarer Runde 1): prüfe in `generate_schedule`, dass `partner_used`/`games_cnt`/`byes_cnt` aus dem Präfix wie in Task 4 gesetzt werden und der Schleifenstart `(n_locked + 1L):R` mit `if (n_locked < R)` geschützt ist. Kein neuer Code erwartet, wenn Task 4 korrekt ist.

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: PASS (alle Tests grün).

- [ ] **Step 5: Commit**

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner.R
git commit -m "test(planner): fixierte Runde 1 wird respektiert + unmoegliche R1 -> NULL"
```

---

### Task 6: Stark+schwach-Bewertung + `reoptimize_tail`

**Files:**
- Modify: `functions/schedule_planner.R`
- Test: `tests/testthat/test-schedule-planner.R`

**Interfaces:**
- Consumes: `generate_schedule`, `verify_schedule`.
- Produces:
  - `schedule_balance_penalty(schedule, strength, from_round = 1L) -> numeric` — Strafe; je Team, dessen beide Spieler auf derselben Seite des Stärke-Medians liegen, +1. `strength` = benannter numerischer Vektor (`names` = player_id als String; höher = stärker).
  - `reoptimize_tail(players, field_sequence, played_rounds, strength, current_schedule, n_candidates = 300L, seed = 1L) -> list` — bester gültiger Gesamtplan (gespielter Präfix + Rest) nach stark+schwach; nie schlechter als `current_schedule`.

- [ ] **Step 1: Failing test schreiben**

Ergänze:

```r
test_that("schedule_balance_penalty zaehlt unbalancierte Teams", {
  # Staerke: 1..4 schwach, 5..8 stark
  strength <- setNames(c(1,1,1,1,9,9,9,9), as.character(1:8))
  good <- list(list(field_count = 2L, byes = integer(0), games = list(
    list(field = 1L, team1 = c(1L, 5L), team2 = c(2L, 6L)),   # je stark+schwach
    list(field = 2L, team1 = c(3L, 7L), team2 = c(4L, 8L)))))
  bad <- list(list(field_count = 2L, byes = integer(0), games = list(
    list(field = 1L, team1 = c(1L, 2L), team2 = c(5L, 6L)),   # je gleiche Seite
    list(field = 2L, team1 = c(3L, 4L), team2 = c(7L, 8L)))))
  expect_equal(schedule_balance_penalty(good, strength), 0)
  expect_gt(schedule_balance_penalty(bad, strength), 0)
})

test_that("reoptimize_tail bleibt valide und nie schlechter", {
  players <- 1:8
  fs <- field_sequence_for(8L, 2L, 5L)
  current <- generate_schedule(players, fs, seed = 1L)
  expect_false(is.null(current))
  strength <- setNames(c(1,2,3,4,9,8,7,6), as.character(1:8))
  played <- list(current[[1]])                      # Runde 1 gilt als gespielt
  best <- reoptimize_tail(players, fs, played, strength, current,
                          n_candidates = 50L, seed = 10L)
  v <- verify_schedule(best, players)
  expect_true(v$ok)
  # nie schlechter als der Ausgangsplan auf dem Rest
  p_cur  <- schedule_balance_penalty(current, strength, from_round = 2L)
  p_best <- schedule_balance_penalty(best,    strength, from_round = 2L)
  expect_lte(p_best, p_cur)
  # gespielte Runde 1 unveraendert
  expect_equal(best[[1]]$games[[1]]$team1, current[[1]]$games[[1]]$team1)
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: FEHLER — `could not find function "schedule_balance_penalty"`.

- [ ] **Step 3: Implementieren**

Ergänze in `functions/schedule_planner.R`:

```r
# Stark+schwach-Strafe: +1 je Team, dessen beide Spieler auf derselben Median-Seite liegen.
schedule_balance_penalty <- function(schedule, strength, from_round = 1L) {
  med <- stats::median(strength)
  pen <- 0
  for (r in seq(from_round, length(schedule))) {
    rd <- schedule[[r]]; if (is.null(rd)) next
    for (gm in rd$games) {
      for (tm in list(gm$team1, gm$team2)) {
        s1 <- strength[as.character(tm[1])]; s2 <- strength[as.character(tm[2])]
        if (isTRUE((s1 <= med) == (s2 <= med))) pen <- pen + 1
      }
    }
  }
  pen
}

# Wählt unter gültigen Rest-Completions die beste für die aktuelle Tabelle.
# current_schedule ist immer Kandidat -> Ergebnis nie schlechter.
reoptimize_tail <- function(players, field_sequence, played_rounds, strength,
                            current_schedule, n_candidates = 300L, seed = 1L) {
  n_played <- length(played_rounds)
  best <- current_schedule
  best_pen <- schedule_balance_penalty(best, strength, from_round = n_played + 1L)
  for (i in seq_len(n_candidates)) {
    cand <- generate_schedule(players, field_sequence, locked_rounds = played_rounds,
                              seed = seed + i)
    if (is.null(cand)) next
    pen <- schedule_balance_penalty(cand, strength, from_round = n_played + 1L)
    if (pen < best_pen) { best <- cand; best_pen <- pen }
  }
  best
}
```

- [ ] **Step 4: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-schedule-planner.R')"`
Expected: PASS (alle Planner-Tests grün).

- [ ] **Step 5: Gesamte Test-Suite + Commit**

Run (volle Suite, muss grün bleiben):
`Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: alle bisherigen + neuen Tests PASS.

```bash
git add functions/schedule_planner.R tests/testthat/test-schedule-planner.R
git commit -m "feat(planner): stark+schwach-Bewertung + reoptimize_tail (monoton)"
```

---

## Self-Review

**1. Spec-Abdeckung (gegen die Design-Spec):**
- §2 Feasibility-Mathematik (variable Felder, R-zentriert, G≤P−1, gleiche Pausen) → Task 2 (`max_games_for`, `field_sequence_for`, `plan_options`, `default_plan_rounds`). ✓
- §3 Generator (konstruktiv + muss-spielen-Regel + Kreis-Fallback) → Task 4; Kreis-Methode Task 3; unabhängiger Verifizierer Task 1. ✓
- §4 Re-Optimierung (mehrere gültige Restpläne, stark+schwach, monoton) → Task 6. Seed/Tabelle wird in Plan B (Integration) angebunden. ✓
- §5 Runde 1 manuell (locked prefix) → Task 5. ✓
- §10 Property-Tests über viele Konfigurationen → Task 4 Sweep + Task 1/5/6. ✓
- §6/§7 State-/UI-Integration (`schedule_mode`, `state$plan`, Setup/Spieltag) → **bewusst NICHT in Plan A**; eigener **Plan B** nach Verifikation dieses Kerns.

**2. Platzhalter-Scan:** keine TBD/TODO; jede Code-Step enthält vollständigen Code. ✓

**3. Typ-Konsistenz:** Runden-Format `list(field_count, games=list(list(field,team1,team2)), byes)` einheitlich in Task 1/4/5/6; `players` = player_id-Vektor durchgehend; `strength` = benannter numerischer Vektor in Task 6 konsistent verwendet. ✓

**Hinweis zur Sättigung mit Pausen:** `generate_schedule` deckt den Kreis-Fallback nur für den No-Pausen-Sättigungsfall (`P = 4·F`, `G = P−1`) ab. Sättigung *mit* Pausen liefert ggf. `NULL`; die App schlägt solche R ohnehin nicht vor (default_plan_rounds zielt auf G 6..8). Dies ist eine bewusste Grenze (Spec §11), kein offener Punkt.

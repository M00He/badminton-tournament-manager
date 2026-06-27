# Badminton Turnier Manager — Phase 1: Kern-Logik & Persistenz-Serialisierung — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Eine vollständig unit-getestete, reine R-Kernlogik (Zustandsmodell, Serialisierung, Auslosungs-Algorithmus, Ergebnis-Validierung, Rangliste) als Fundament für den späteren UI-Umbau.

**Architecture:** Ein einziges serialisierbares State-Objekt ist die Quelle der Wahrheit. Alle Zustandsänderungen laufen über reine, getestete Mutations-Funktionen (`ts_*`). Der neue Auslosungsalgorithmus erzeugt vollständige Runden-Kandidaten, bewertet sie mit einer gewichteten Straffunktion entlang der Prioritäten-Hierarchie und wählt deterministisch (per Seed) die beste. Keine Shiny-Abhängigkeit in dieser Phase.

**Tech Stack:** R 4.4.1, `jsonlite` (Serialisierung), `testthat` (Tests). Keine weiteren neuen Laufzeit-Abhängigkeiten.

## Global Constraints

- **Plattform:** R 4.4.1. Pakete über `renv` pinnen (`renv.lock` aktualisieren).
- **Tests ausführen unter Windows:** ausschließlich via `Rscript` über **PowerShell**, nicht git-bash (segfault-Risiko im R-Stack). Beispiel: `Rscript -e "testthat::test_dir('tests/testthat')"`.
- **Stabile IDs:** Spieler werden über `player_id` (Integer) referenziert, niemals über den Namen. Spiele über `game_id` (Integer).
- **Reine Funktionen:** Mutations-Funktionen nehmen `state` und geben ein **neues** `state` zurück. Kein `<<-`, keine globalen Variablen, keine Seiteneffekte.
- **Determinismus:** Jede zufallsbehaftete Funktion akzeptiert einen `seed`; gleicher Seed + gleicher Input ⇒ gleiches Ergebnis.
- **Punkte-Semantik:** `t1_points`/`t2_points` = ranglistenrelevanter Ergebniswert. Bei `best_of_3_11` = gewonnene Sätze; bei Einzelsatz-Systemen = erzielte Punkte. Einheitlich dokumentiert.
- **Commits:** klein und häufig, ein Commit pro Task-Abschluss.
- **Spec:** `docs/superpowers/specs/2026-06-27-badminton-turnier-redesign-design.md`.

---

## Schnittstellen-Überblick (kanonische Signaturen)

`functions/tournament_state.R`
- `SCHEMA_VERSION <- 2L`
- `empty_players_df()`, `empty_games_df()`
- `new_tournament_state(name = NULL, created_at = NULL)`
- `ts_add_player(state, name, gender)`
- `ts_rename_player(state, player_id, new_name, new_gender)`
- `ts_set_player_active(state, player_id, active)`
- `ts_start_tournament(state, num_rounds, num_fields, game_system)`
- `ts_active_players(state)`
- `ts_set_round_games(state, round, pairings)`
- `ts_save_result(state, game_id, t1_sets, t2_sets)`
- `ts_lock_round(state, round)`
- `ts_advance_round(state)`
- `state_to_json(state)`, `state_from_json(json)`, `migrate_state(raw)`

`functions/game_system.R`
- `get_game_system_info(system_type)`
- `sets_won_from_scores(t1_sets, t2_sets)`
- `validate_best_of_3(t1_sets, t2_sets, system_type)`
- `validate_single_set(points1, points2, system_type)`
- `format_game_system(system_type)`

`functions/ranking_calculation.R`
- `calculate_player_stats(games, player_ids)`
- `get_direct_comparison(id1, id2, games)`
- `create_ranking(games, player_ids)`

`functions/draw_engine.R`
- `get_partnership_history(games, before_round)`
- `get_opponent_history(games, before_round)`
- `get_opponent_team_history(games, before_round)`
- `get_previous_round_opponents(games, round)`
- `count_games_played(games, player_ids, before_round)`
- `select_round_players(state, round, ranking)`
- `generate_candidate(players, better_half, worse_half, num_fields)`
- `score_draw(pairings, histories, ranking)`
- `generate_round_draw(state, round, seed = 1L, n_candidates = 300L)`

---

## Task 1: Test-Infrastruktur + State-Schema & Konstruktoren

**Files:**
- Create: `functions/tournament_state.R`
- Create: `tests/testthat/test-tournament_state.R`
- Create: `tests/testthat.R`

**Interfaces:**
- Produces: `SCHEMA_VERSION`, `empty_players_df()`, `empty_games_df()`, `new_tournament_state(name, created_at)`.

- [ ] **Step 1: Test-Runner anlegen**

`tests/testthat.R`:
```r
library(testthat)
for (f in list.files("functions", pattern = "\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
testthat::test_dir("tests/testthat")
```

- [ ] **Step 2: Failing test schreiben**

`tests/testthat/test-tournament_state.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")

test_that("new_tournament_state hat leere, korrekt typisierte Struktur", {
  s <- new_tournament_state(name = "Test", created_at = "2026-06-27T10:00:00")
  expect_equal(s$schema_version, 2L)
  expect_equal(s$tournament_name, "Test")
  expect_equal(s$status, "setup")
  expect_equal(s$current_round, 1L)
  expect_equal(nrow(s$players), 0L)
  expect_equal(nrow(s$games), 0L)
  expect_true(all(c("player_id", "name", "gender", "active") %in% names(s$players)))
  expect_true(all(c("game_id", "round", "field",
                    "t1_p1", "t1_p2", "t2_p1", "t2_p2",
                    "t1_points", "t2_points", "locked") %in% names(s$games)))
})
```

- [ ] **Step 3: Test laufen lassen (muss fehlschlagen)**

Run (PowerShell): `Rscript -e "testthat::test_file('tests/testthat/test-tournament_state.R')"`
Expected: FAIL — `could not find function "new_tournament_state"`.

- [ ] **Step 4: Implementierung schreiben**

`functions/tournament_state.R`:
```r
# Tournament State — Schema, Konstruktoren, Mutationen, Serialisierung

SCHEMA_VERSION <- 2L

empty_players_df <- function() {
  data.frame(
    player_id = integer(),
    name      = character(),
    gender    = character(),   # "m" | "w"
    active    = logical(),
    stringsAsFactors = FALSE
  )
}

empty_games_df <- function() {
  data.frame(
    game_id = integer(), round = integer(), field = integer(),
    t1_p1 = integer(), t1_p2 = integer(), t2_p1 = integer(), t2_p2 = integer(),
    t1_set1 = integer(), t2_set1 = integer(),
    t1_set2 = integer(), t2_set2 = integer(),
    t1_set3 = integer(), t2_set3 = integer(),
    t1_points = integer(), t2_points = integer(),
    locked = logical(),
    stringsAsFactors = FALSE
  )
}

new_tournament_state <- function(name = NULL, created_at = NULL) {
  list(
    schema_version  = SCHEMA_VERSION,
    tournament_name = if (is.null(name)) "" else name,
    created_at      = if (is.null(created_at)) "" else created_at,
    settings        = list(num_rounds = 5L, num_fields = 4L,
                           game_system = "best_of_3_11"),
    status          = "setup",         # "setup" | "running" | "finished"
    current_round   = 1L,
    players         = empty_players_df(),
    games           = empty_games_df()
  )
}
```

- [ ] **Step 5: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-tournament_state.R')"`
Expected: PASS.

- [ ] **Step 6: testthat in renv aufnehmen & committen**

Run: `Rscript -e "renv::install('testthat'); renv::snapshot()"`
```bash
git add functions/tournament_state.R tests/testthat.R tests/testthat/test-tournament_state.R renv.lock
git commit -m "feat(state): State-Schema, Konstruktoren und Test-Infrastruktur"
```

---

## Task 2: Spieler-Mutationen (stabile IDs)

**Files:**
- Modify: `functions/tournament_state.R`
- Modify: `tests/testthat/test-tournament_state.R`

**Interfaces:**
- Consumes: `new_tournament_state()`, `empty_players_df()`.
- Produces: `ts_add_player(state, name, gender)`, `ts_rename_player(state, player_id, new_name, new_gender)`, `ts_set_player_active(state, player_id, active)`, `ts_active_players(state)`.

- [ ] **Step 1: Failing tests schreiben** (an `test-tournament_state.R` anhängen)

```r
test_that("ts_add_player vergibt stabile, aufsteigende IDs und verhindert Duplikate", {
  s <- new_tournament_state()
  s <- ts_add_player(s, "Anna", "w")
  s <- ts_add_player(s, "Ben", "m")
  expect_equal(s$players$player_id, c(1L, 2L))
  expect_equal(s$players$name, c("Anna", "Ben"))
  expect_true(all(s$players$active))
  expect_error(ts_add_player(s, "Anna", "w"), "existiert bereits")
})

test_that("ts_rename_player ändert Name/Geschlecht ohne ID-Wechsel", {
  s <- ts_add_player(new_tournament_state(), "Anna", "w")
  s <- ts_rename_player(s, 1L, "Anna B.", "w")
  expect_equal(s$players$player_id, 1L)
  expect_equal(s$players$name, "Anna B.")
})

test_that("ts_set_player_active schaltet aktiv/inaktiv; ts_active_players filtert", {
  s <- ts_add_player(ts_add_player(new_tournament_state(), "Anna", "w"), "Ben", "m")
  s <- ts_set_player_active(s, 2L, FALSE)
  expect_equal(ts_active_players(s)$player_id, 1L)
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-tournament_state.R')"`
Expected: FAIL — `ts_add_player` nicht gefunden.

- [ ] **Step 3: Implementierung anhängen** (`functions/tournament_state.R`)

```r
.next_id <- function(ids) if (length(ids) == 0) 1L else max(ids) + 1L

ts_add_player <- function(state, name, gender) {
  name <- trimws(name)
  if (name == "") stop("Name darf nicht leer sein.")
  if (name %in% state$players$name) stop("Spieler existiert bereits.")
  if (!gender %in% c("m", "w")) stop("Geschlecht muss 'm' oder 'w' sein.")
  new_row <- data.frame(
    player_id = .next_id(state$players$player_id),
    name = name, gender = gender, active = TRUE,
    stringsAsFactors = FALSE
  )
  state$players <- rbind(state$players, new_row)
  state
}

ts_rename_player <- function(state, player_id, new_name, new_gender) {
  new_name <- trimws(new_name)
  if (new_name == "") stop("Name darf nicht leer sein.")
  idx <- which(state$players$player_id == player_id)
  if (length(idx) == 0) stop("Spieler nicht gefunden.")
  clash <- new_name %in% state$players$name[state$players$player_id != player_id]
  if (clash) stop("Name existiert bereits.")
  state$players$name[idx] <- new_name
  state$players$gender[idx] <- new_gender
  state
}

ts_set_player_active <- function(state, player_id, active) {
  idx <- which(state$players$player_id == player_id)
  if (length(idx) == 0) stop("Spieler nicht gefunden.")
  state$players$active[idx] <- isTRUE(active)
  state
}

ts_active_players <- function(state) {
  state$players[isTRUE(state$players$active) | state$players$active, , drop = FALSE]
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-tournament_state.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add functions/tournament_state.R tests/testthat/test-tournament_state.R
git commit -m "feat(state): Spieler-Mutationen mit stabilen IDs"
```

---

## Task 3: Turnierstart & Spiel-Mutationen

**Files:**
- Modify: `functions/tournament_state.R`
- Modify: `tests/testthat/test-tournament_state.R`

**Interfaces:**
- Consumes: `ts_add_player()`, `empty_games_df()`.
- Produces: `ts_start_tournament(state, num_rounds, num_fields, game_system)`, `ts_set_round_games(state, round, pairings)`, `ts_save_result(state, game_id, t1_sets, t2_sets)`, `ts_lock_round(state, round)`, `ts_advance_round(state)`.
- `pairings` ist eine Liste von `list(field, team1 = c(id, id), team2 = c(id, id))`.

- [ ] **Step 1: Failing tests schreiben**

```r
make_started <- function(n = 8) {
  s <- new_tournament_state()
  for (i in seq_len(n)) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  ts_start_tournament(s, num_rounds = 5L, num_fields = 2L, game_system = "best_of_3_11")
}

test_that("ts_start_tournament setzt Status und Einstellungen", {
  s <- make_started()
  expect_equal(s$status, "running")
  expect_equal(s$settings$num_fields, 2L)
  expect_equal(s$current_round, 1L)
})

test_that("ts_set_round_games schreibt Felder mit NA-Ergebnis, nicht gesperrt", {
  s <- make_started()
  pairings <- list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L)))
  s <- ts_set_round_games(s, 1L, pairings)
  g <- s$games[s$games$round == 1L & s$games$field == 1L, ]
  expect_equal(nrow(g), 1L)
  expect_equal(c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2), c(1L, 2L, 3L, 4L))
  expect_true(is.na(g$t1_points))
  expect_false(g$locked)
})

test_that("ts_save_result berechnet gewonnene Sätze bei best_of_3", {
  s <- make_started()
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  gid <- s$games$game_id[1]
  s <- ts_save_result(s, gid, t1_sets = c(11L, 8L, 11L), t2_sets = c(7L, 11L, 9L))
  g <- s$games[s$games$game_id == gid, ]
  expect_equal(g$t1_points, 2L)   # 2 Sätze gewonnen
  expect_equal(g$t2_points, 1L)
})

test_that("ts_advance_round nur bei komplett gesperrter, vollständiger Runde", {
  s <- make_started()
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  expect_error(ts_advance_round(s), "nicht abgeschlossen")
  gid <- s$games$game_id[1]
  s <- ts_save_result(s, gid, c(11L, 11L, NA), c(5L, 7L, NA))
  s <- ts_lock_round(s, 1L)
  s <- ts_advance_round(s)
  expect_equal(s$current_round, 2L)
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-tournament_state.R')"`
Expected: FAIL — `ts_start_tournament` nicht gefunden.

- [ ] **Step 3: Implementierung anhängen**

```r
ts_start_tournament <- function(state, num_rounds, num_fields, game_system) {
  if (nrow(ts_active_players(state)) < 4) stop("Mindestens 4 aktive Spieler benötigt.")
  state$settings <- list(num_rounds = as.integer(num_rounds),
                         num_fields = as.integer(num_fields),
                         game_system = game_system)
  state$current_round <- 1L
  state$status <- "running"
  state$games <- empty_games_df()
  state
}

ts_set_round_games <- function(state, round, pairings) {
  round <- as.integer(round)
  if (any(state$games$round == round & state$games$locked)) {
    stop("Runde ist gesperrt und kann nicht neu ausgelost werden.")
  }
  state$games <- state$games[state$games$round != round, , drop = FALSE]
  for (p in pairings) {
    row <- empty_games_df()[1, ]
    row$game_id <- .next_id(state$games$game_id)
    row$round <- round; row$field <- as.integer(p$field)
    row$t1_p1 <- p$team1[1]; row$t1_p2 <- p$team1[2]
    row$t2_p1 <- p$team2[1]; row$t2_p2 <- p$team2[2]
    row$locked <- FALSE
    state$games <- rbind(state$games, row)
  }
  state
}

# t1_sets/t2_sets: Länge-3-Vektoren (Best-of-3) ODER Länge-1 (Einzelsatz).
ts_save_result <- function(state, game_id, t1_sets, t2_sets) {
  idx <- which(state$games$game_id == game_id)
  if (length(idx) == 0) stop("Spiel nicht gefunden.")
  if (state$games$locked[idx]) stop("Spiel ist gesperrt.")
  sets <- sets_won_from_scores(t1_sets, t2_sets)
  state$games$t1_set1[idx] <- t1_sets[1]; state$games$t2_set1[idx] <- t2_sets[1]
  state$games$t1_set2[idx] <- if (length(t1_sets) >= 2) t1_sets[2] else NA_integer_
  state$games$t2_set2[idx] <- if (length(t2_sets) >= 2) t2_sets[2] else NA_integer_
  state$games$t1_set3[idx] <- if (length(t1_sets) >= 3) t1_sets[3] else NA_integer_
  state$games$t2_set3[idx] <- if (length(t2_sets) >= 3) t2_sets[3] else NA_integer_
  state$games$t1_points[idx] <- sets[1]
  state$games$t2_points[idx] <- sets[2]
  state
}

ts_lock_round <- function(state, round) {
  round <- as.integer(round)
  rows <- state$games$round == round
  if (!any(rows)) stop("Keine Spiele in dieser Runde.")
  if (any(is.na(state$games$t1_points[rows]) | is.na(state$games$t2_points[rows]))) {
    stop("Runde nicht abgeschlossen: es fehlen Ergebnisse.")
  }
  state$games$locked[rows] <- TRUE
  state
}

ts_advance_round <- function(state) {
  round <- state$current_round
  rows <- state$games$round == round
  if (!any(rows) || !all(state$games$locked[rows])) {
    stop("Aktuelle Runde nicht abgeschlossen.")
  }
  if (round >= state$settings$num_rounds) {
    state$status <- "finished"
  } else {
    state$current_round <- round + 1L
  }
  state
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)** — `sets_won_from_scores` kommt aus Task 4; bis dahin Test mit `skip_if_not(exists("sets_won_from_scores"))` absichern ODER Task 4 zuerst mergen. Reihenfolge-Hinweis: Task 4 unmittelbar nach Task 3 ausführen.

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS (nach Task 4).

- [ ] **Step 5: Commit**

```bash
git add functions/tournament_state.R tests/testthat/test-tournament_state.R
git commit -m "feat(state): Turnierstart und Spiel-Mutationen (set/save/lock/advance)"
```

---

## Task 4: Spielsystem & Ergebnis-Validierung (`game_system.R`)

**Files:**
- Create: `functions/game_system.R` (ersetzt `functions/game_system_validation.R`)
- Create: `tests/testthat/test-game_system.R`
- Delete: `functions/game_system_validation.R`

**Interfaces:**
- Produces: `get_game_system_info`, `sets_won_from_scores`, `validate_best_of_3`, `validate_single_set`, `format_game_system`.

- [ ] **Step 1: Failing tests schreiben**

`tests/testthat/test-game_system.R`:
```r
source("../../functions/game_system.R", encoding = "UTF-8")

test_that("sets_won_from_scores zählt Satzsiege, ignoriert NA und Gleichstände", {
  expect_equal(sets_won_from_scores(c(11L, 8L, 11L), c(7L, 11L, 9L)), c(2L, 1L))
  expect_equal(sets_won_from_scores(15L, 10L), c(1L, 0L))
  expect_equal(sets_won_from_scores(c(11L, NA, NA), c(9L, NA, NA)), c(1L, 0L))
})

test_that("validate_best_of_3 erkennt gültige und ungültige Resultate", {
  expect_true(validate_best_of_3(c(11L, 11L), c(7L, 9L), "best_of_3_11")$valid)
  # kein Gewinner mit 2 Sätzen:
  expect_false(validate_best_of_3(c(11L, 5L, NA), c(7L, 11L, NA), "best_of_3_11")$valid)
  # Satz über max 15 hinaus:
  expect_false(validate_best_of_3(c(16L, 11L), c(14L, 5L), "best_of_3_11")$valid)
})

test_that("validate_single_set prüft Mindestpunkte und Differenz", {
  expect_true(validate_single_set(15L, 10L, "single_15")$valid)
  expect_false(validate_single_set(14L, 12L, "single_15")$valid)  # < min
  expect_false(validate_single_set(15L, 15L, "single_15")$valid)  # kein Gewinner
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-game_system.R')"`
Expected: FAIL — Datei/Funktion fehlt.

- [ ] **Step 3: Implementierung schreiben**

`functions/game_system.R`:
```r
# Spielsysteme & Ergebnis-Validierung

get_game_system_info <- function(system_type) {
  systems <- list(
    best_of_3_11 = list(name = "Zwei Gewinnsätze bis 11",
      description = "Best of 3, Sätze bis 11 (2 Pkt. Differenz, max 15:14)",
      min_points = 11L, min_difference = 2L, max_points = 15L, is_best_of_3 = TRUE),
    single_15 = list(name = "Ein Satz bis 15",
      description = "Ein Satz bis 15 (2 Pkt. Differenz, max 21:20)",
      min_points = 15L, min_difference = 2L, max_points = 21L, is_best_of_3 = FALSE),
    single_21 = list(name = "Ein Satz bis 21",
      description = "Ein Satz bis 21 (2 Pkt. Differenz, max 30:29)",
      min_points = 21L, min_difference = 2L, max_points = 30L, is_best_of_3 = FALSE),
    single_30 = list(name = "Ein Satz bis 30",
      description = "Ein Satz bis 30 (max 30:29)",
      min_points = 30L, min_difference = 2L, max_points = 30L, is_best_of_3 = FALSE)
  )
  systems[[system_type]]
}

format_game_system <- function(system_type) {
  info <- get_game_system_info(system_type)
  if (is.null(info)) "Unbekanntes System" else info$description
}

sets_won_from_scores <- function(t1_sets, t2_sets) {
  t1 <- 0L; t2 <- 0L
  for (i in seq_along(t1_sets)) {
    a <- t1_sets[i]; b <- t2_sets[i]
    if (is.na(a) || is.na(b)) next
    if (a > b) t1 <- t1 + 1L else if (b > a) t2 <- t2 + 1L
  }
  c(t1, t2)
}

.valid_set_score <- function(hi, lo, info) {
  if (hi < info$min_points) return(FALSE)
  if (hi > info$max_points) return(FALSE)
  diff <- hi - lo
  if (hi == info$min_points) return(diff >= info$min_difference)
  # über min_points: Differenz genau 2 (außer am Deckel max_points)
  if (hi == info$max_points) return(diff >= 1L)
  diff == info$min_difference
}

validate_single_set <- function(points1, points2, system_type) {
  info <- get_game_system_info(system_type)
  if (is.null(info)) return(list(valid = FALSE, message = "Unbekanntes Spielsystem."))
  if (is.na(points1) || is.na(points2) || points1 < 0 || points2 < 0)
    return(list(valid = FALSE, message = "Punkte müssen nicht-negativ sein."))
  if (points1 == points2)
    return(list(valid = FALSE, message = "Es muss einen Gewinner geben."))
  hi <- max(points1, points2); lo <- min(points1, points2)
  if (!.valid_set_score(hi, lo, info))
    return(list(valid = FALSE, message = "Ergebnis verletzt die Systemregeln."))
  list(valid = TRUE, message = "")
}

validate_best_of_3 <- function(t1_sets, t2_sets, system_type) {
  info <- get_game_system_info(system_type)
  if (is.null(info) || !info$is_best_of_3)
    return(list(valid = FALSE, message = "Kein Best-of-3-System."))
  played <- which(!is.na(t1_sets) & !is.na(t2_sets))
  if (length(played) < 2)
    return(list(valid = FALSE, message = "Mindestens 2 gespielte Sätze nötig."))
  for (i in played) {
    if (t1_sets[i] == t2_sets[i])
      return(list(valid = FALSE, message = "Ein Satz braucht einen Gewinner."))
    hi <- max(t1_sets[i], t2_sets[i]); lo <- min(t1_sets[i], t2_sets[i])
    if (!.valid_set_score(hi, lo, info))
      return(list(valid = FALSE, message = paste("Satz", i, "verletzt die Regeln.")))
  }
  sets <- sets_won_from_scores(t1_sets, t2_sets)
  if (max(sets) != 2L)
    return(list(valid = FALSE, message = "Der Gewinner muss genau 2 Sätze haben."))
  if (min(sets) > 1L)
    return(list(valid = FALSE, message = "Der Verlierer kann maximal 1 Satz haben."))
  list(valid = TRUE, message = "")
}
```

- [ ] **Step 4: Alte Datei entfernen, Tests laufen lassen**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"` (jetzt sollten auch Task-3-Tests grün sein)
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git rm functions/game_system_validation.R
git add functions/game_system.R tests/testthat/test-game_system.R
git commit -m "feat(rules): aktive Ergebnis-Validierung für Einzelsatz und Best-of-3"
```

---

## Task 5: Rangliste — ID-basiert & gehärtet (`ranking_calculation.R`)

**Files:**
- Modify: `functions/ranking_calculation.R`
- Create: `tests/testthat/test-ranking.R`

**Interfaces:**
- Consumes: `empty_games_df()`-Spalten (`t1_p1..t2_p2`, `t1_points`, `t2_points`).
- Produces: `calculate_player_stats(games, player_ids)`, `get_direct_comparison(id1, id2, games)`, `create_ranking(games, player_ids)` → Data Frame mit Spalten `rank, player_id, games_played, wins, losses, points_for, points_against, point_diff`.

- [ ] **Step 1: Failing tests schreiben**

`tests/testthat/test-ranking.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")

make_games <- function() {
  g <- empty_games_df()
  g <- rbind(g, transform(empty_games_df()[1, ],
    game_id = 1L, round = 1L, field = 1L,
    t1_p1 = 1L, t1_p2 = 2L, t2_p1 = 3L, t2_p2 = 4L,
    t1_points = 2L, t2_points = 0L, locked = TRUE))
  g
}

test_that("create_ranking zählt Siege/Niederlagen pro player_id", {
  r <- create_ranking(make_games(), c(1L, 2L, 3L, 4L))
  expect_equal(r$wins[r$player_id == 1L], 1L)
  expect_equal(r$losses[r$player_id == 3L], 1L)
  expect_equal(r$rank[1], 1L)
})

test_that("create_ranking ohne Ergebnisse liefert Nullzeilen sauber", {
  r <- create_ranking(empty_games_df(), c(1L, 2L))
  expect_equal(nrow(r), 2L)
  expect_true(all(r$games_played == 0L))
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-ranking.R')"`
Expected: FAIL (alte Funktion ist namensbasiert / Signatur passt nicht).

- [ ] **Step 3: Implementierung ersetzen** (`functions/ranking_calculation.R` komplett)

```r
# Rangliste — ID-basiert

calculate_player_stats <- function(games, player_ids) {
  stats <- data.frame(player_id = player_ids, games_played = 0L, wins = 0L,
                      losses = 0L, points_for = 0L, points_against = 0L,
                      point_diff = 0L, stringsAsFactors = FALSE)
  if (nrow(games) == 0) return(stats)
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    t1_won <- g$t1_points > g$t2_points
    upd <- function(stats, ids, pf, pa, won) {
      for (id in ids) {
        k <- which(stats$player_id == id); if (!length(k)) next
        stats$games_played[k] <- stats$games_played[k] + 1L
        stats$points_for[k]   <- stats$points_for[k] + pf
        stats$points_against[k] <- stats$points_against[k] + pa
        if (won) stats$wins[k] <- stats$wins[k] + 1L
        else stats$losses[k] <- stats$losses[k] + 1L
      }
      stats
    }
    stats <- upd(stats, t1, g$t1_points, g$t2_points, t1_won)
    stats <- upd(stats, t2, g$t2_points, g$t1_points, !t1_won)
  }
  stats$point_diff <- stats$points_for - stats$points_against
  stats
}

get_direct_comparison <- function(id1, id2, games) {
  if (nrow(games) == 0) return(0L)
  w1 <- 0L; w2 <- 0L
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    if (is.na(g$t1_points) || is.na(g$t2_points)) next
    t1 <- c(g$t1_p1, g$t1_p2); t2 <- c(g$t2_p1, g$t2_p2)
    opp <- (id1 %in% t1 && id2 %in% t2) || (id1 %in% t2 && id2 %in% t1)
    if (!opp) next
    t1_won <- g$t1_points > g$t2_points
    if ((id1 %in% t1 && t1_won) || (id1 %in% t2 && !t1_won)) w1 <- w1 + 1L else w2 <- w2 + 1L
  }
  if (w1 > w2) 1L else if (w2 > w1) -1L else 0L
}

create_ranking <- function(games, player_ids) {
  stats <- calculate_player_stats(games, player_ids)
  if (nrow(stats) == 0) { stats$rank <- integer(); return(stats) }
  stats <- stats[order(-stats$wins, -stats$point_diff), ]
  n <- nrow(stats)
  if (n > 1) for (i in 1:(n - 1)) for (j in (i + 1):n) {
    if (stats$wins[i] == stats$wins[j] && stats$point_diff[i] == stats$point_diff[j]) {
      if (get_direct_comparison(stats$player_id[i], stats$player_id[j], games) < 0) {
        tmp <- stats[i, ]; stats[i, ] <- stats[j, ]; stats[j, ] <- tmp
      }
    }
  }
  stats$rank <- seq_len(nrow(stats))
  stats[, c("rank", "player_id", "games_played", "wins", "losses",
            "points_for", "points_against", "point_diff")]
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add functions/ranking_calculation.R tests/testthat/test-ranking.R
git commit -m "feat(ranking): ID-basierte Rangliste mit Tests"
```

---

## Task 6: Draw-Engine — Historien & Spielzählung

**Files:**
- Create: `functions/draw_engine.R`
- Create: `tests/testthat/test-draw_engine.R`

**Interfaces:**
- Consumes: `empty_games_df()`-Spalten.
- Produces: `get_partnership_history(games, before_round)`, `get_opponent_history(games, before_round)`, `get_opponent_team_history(games, before_round)`, `get_previous_round_opponents(games, round)`, `count_games_played(games, player_ids, before_round)`. Alle Historien sind benannte Listen `player_id (als character) → vector`.

- [ ] **Step 1: Failing tests schreiben**

`tests/testthat/test-draw_engine.R`:
```r
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/draw_engine.R", encoding = "UTF-8")

two_round_games <- function() {
  g <- empty_games_df()
  add <- function(g, gid, rnd, fld, a, b, c, d) rbind(g, transform(empty_games_df()[1, ],
    game_id = gid, round = rnd, field = fld,
    t1_p1 = a, t1_p2 = b, t2_p1 = c, t2_p2 = d,
    t1_points = 2L, t2_points = 0L, locked = TRUE))
  g <- add(g, 1L, 1L, 1L, 1L, 2L, 3L, 4L)
  g <- add(g, 2L, 1L, 2L, 5L, 6L, 7L, 8L)
  g
}

test_that("get_partnership_history erfasst Partner beidseitig", {
  h <- get_partnership_history(two_round_games(), before_round = 99L)
  expect_equal(h[["1"]], 2L)
  expect_equal(h[["2"]], 1L)
})

test_that("count_games_played zählt nur Runden < before_round", {
  c1 <- count_games_played(two_round_games(), 1:8, before_round = 2L)
  expect_equal(unname(c1["1"]), 1L)
  c0 <- count_games_played(two_round_games(), 1:8, before_round = 1L)
  expect_equal(unname(c0["1"]), 0L)
})

test_that("get_previous_round_opponents liefert Gegner aus round-1", {
  h <- get_previous_round_opponents(two_round_games(), round = 2L)
  expect_setequal(h[["1"]], c(3L, 4L))
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-draw_engine.R')"`
Expected: FAIL — Funktionen fehlen.

- [ ] **Step 3: Implementierung schreiben** (`functions/draw_engine.R`)

```r
# Auslosungs-Algorithmus (Score-and-Select)

.games_before <- function(games, before_round) {
  if (nrow(games) == 0) return(games)
  games[games$round < before_round, , drop = FALSE]
}

get_partnership_history <- function(games, before_round) {
  g <- .games_before(games, before_round); h <- list()
  push <- function(h, a, b) { k <- as.character(a); h[[k]] <- c(h[[k]], b); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]
    h <- push(h, r$t1_p1, r$t1_p2); h <- push(h, r$t1_p2, r$t1_p1)
    h <- push(h, r$t2_p1, r$t2_p2); h <- push(h, r$t2_p2, r$t2_p1)
  }
  h
}

get_opponent_history <- function(games, before_round) {
  g <- .games_before(games, before_round); h <- list()
  push <- function(h, a, opps) { k <- as.character(a); h[[k]] <- c(h[[k]], opps); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]; t1 <- c(r$t1_p1, r$t1_p2); t2 <- c(r$t2_p1, r$t2_p2)
    for (p in t1) h <- push(h, p, t2); for (p in t2) h <- push(h, p, t1)
  }
  h
}

get_opponent_team_history <- function(games, before_round) {
  g <- .games_before(games, before_round); h <- list()
  tid <- function(x) paste(sort(x), collapse = "|")
  push <- function(h, a, id) { k <- as.character(a); h[[k]] <- c(h[[k]], id); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]; t1 <- c(r$t1_p1, r$t1_p2); t2 <- c(r$t2_p1, r$t2_p2)
    for (p in t1) h <- push(h, p, tid(t2)); for (p in t2) h <- push(h, p, tid(t1))
  }
  h
}

get_previous_round_opponents <- function(games, round) {
  if (round <= 1) return(list())
  g <- games[games$round == (round - 1L), , drop = FALSE]; h <- list()
  push <- function(h, a, opps) { k <- as.character(a); h[[k]] <- c(h[[k]], opps); h }
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]; t1 <- c(r$t1_p1, r$t1_p2); t2 <- c(r$t2_p1, r$t2_p2)
    for (p in t1) h <- push(h, p, t2); for (p in t2) h <- push(h, p, t1)
  }
  h
}

count_games_played <- function(games, player_ids, before_round) {
  counts <- setNames(rep(0L, length(player_ids)), as.character(player_ids))
  g <- .games_before(games, before_round)
  for (i in seq_len(nrow(g))) {
    r <- g[i, ]
    for (id in c(r$t1_p1, r$t1_p2, r$t2_p1, r$t2_p2)) {
      k <- as.character(id)
      if (k %in% names(counts)) counts[k] <- counts[k] + 1L
    }
  }
  counts
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-draw_engine.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add functions/draw_engine.R tests/testthat/test-draw_engine.R
git commit -m "feat(draw): Historien und Spielzählung (ID-basiert)"
```

---

## Task 7: Draw-Engine — Spielerauswahl & Kandidatenerzeugung

**Files:**
- Modify: `functions/draw_engine.R`
- Modify: `tests/testthat/test-draw_engine.R`

**Interfaces:**
- Consumes: `count_games_played()`, `create_ranking()`, `ts_active_players()`.
- Produces: `select_round_players(state, round, ranking)` → `list(playing = <int ids>, byes = <int ids>)`; `generate_candidate(players, better_half, worse_half, num_fields)` → `pairings`-Liste `list(field, team1, team2)`.

- [ ] **Step 1: Failing tests schreiben**

```r
test_that("select_round_players bevorzugt Spieler mit wenigsten Spielen", {
  s <- new_tournament_state()
  for (i in 1:6) s <- ts_add_player(s, paste("P", i), "m")
  s <- ts_start_tournament(s, 5L, 1L, "best_of_3_11")  # 1 Feld => 4 spielen, 2 byes
  # P1..P4 haben in Runde 1 gespielt:
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L,11L), c(5L,7L))
  s <- ts_lock_round(s, 1L)
  rk <- create_ranking(s$games, ts_active_players(s)$player_id)
  sel <- select_round_players(s, round = 2L, ranking = rk)
  expect_length(sel$playing, 4L)
  expect_true(all(c(5L, 6L) %in% sel$playing))  # die mit 0 Spielen müssen rein
})

test_that("generate_candidate erzeugt vollständige, disjunkte Felder", {
  cand <- generate_candidate(players = 1:8, better_half = 1:4, worse_half = 5:8, num_fields = 2L)
  ids <- unlist(lapply(cand, function(p) c(p$team1, p$team2)))
  expect_length(ids, 8L)
  expect_length(unique(ids), 8L)
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-draw_engine.R')"`
Expected: FAIL — Funktionen fehlen.

- [ ] **Step 3: Implementierung anhängen**

```r
select_round_players <- function(state, round, ranking) {
  active <- ts_active_players(state)$player_id
  n_cap <- state$settings$num_fields * 4L
  n_play <- min(length(active), n_cap)
  n_play <- (n_play %/% 4L) * 4L
  gp <- count_games_played(state$games, active, before_round = round)
  rank_of <- function(id) {
    k <- which(ranking$player_id == id)
    if (length(k)) ranking$rank[k] else 9999L
  }
  ord <- order(gp[as.character(active)], vapply(active, rank_of, integer(1)))
  playing <- active[ord][seq_len(n_play)]
  byes <- setdiff(active, playing)
  list(playing = playing, byes = byes)
}

generate_candidate <- function(players, better_half, worse_half, num_fields) {
  better <- sample(intersect(better_half, players))
  worse  <- sample(intersect(worse_half, players))
  # Auffüllen, falls Hälften unsymmetrisch (z. B. durch Aussetzer)
  pool <- sample(players)
  take <- function(vec, n) { out <- vec[seq_len(n)]; out }
  pairings <- list()
  bi <- 1L; wi <- 1L
  for (f in seq_len(num_fields)) {
    quad <- c(better[bi], worse[wi], better[bi + 1L], worse[wi + 1L])
    bi <- bi + 2L; wi <- wi + 2L
    if (any(is.na(quad))) {                 # Fallback: aus Restpool ziehen
      used <- unlist(lapply(pairings, function(p) c(p$team1, p$team2)))
      rest <- setdiff(pool, c(used, quad[!is.na(quad)]))
      quad[is.na(quad)] <- rest[seq_len(sum(is.na(quad)))]
    }
    pairings[[f]] <- list(field = f, team1 = c(quad[1], quad[2]),
                          team2 = c(quad[3], quad[4]))
  }
  pairings
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-draw_engine.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add functions/draw_engine.R tests/testthat/test-draw_engine.R
git commit -m "feat(draw): Spielerauswahl (Prio 0) und Kandidatenerzeugung"
```

---

## Task 8: Draw-Engine — Scoring & Auswahl der besten Auslosung

**Files:**
- Modify: `functions/draw_engine.R`
- Modify: `tests/testthat/test-draw_engine.R`

**Interfaces:**
- Consumes: `get_partnership_history()`, `get_previous_round_opponents()`, `get_opponent_team_history()`, `get_opponent_history()`, `create_ranking()`, `select_round_players()`, `generate_candidate()`.
- Produces: `score_draw(pairings, histories, ranking)` → `list(penalty, satisfied)`; `generate_round_draw(state, round, seed, n_candidates)` → `list(pairings, byes, penalty, quality)`.
- `histories` ist `list(partner=, prev=, team=, opp=)`. `quality` ist Character-Vektor erfüllter Prioritäten.

- [ ] **Step 1: Failing tests schreiben**

```r
test_that("score_draw bestraft Partner-Wiederholung am höchsten", {
  hist <- list(partner = list("1" = 2L), prev = list(), team = list(), opp = list())
  rk <- data.frame(player_id = 1:8, rank = 1:8)
  repeat_partner <- list(list(field = 1L, team1 = c(1L, 2L), team2 = c(3L, 4L)))
  fresh        <- list(list(field = 1L, team1 = c(1L, 5L), team2 = c(3L, 4L)))
  expect_gt(score_draw(repeat_partner, hist, rk)$penalty,
            score_draw(fresh, hist, rk)$penalty)
})

test_that("generate_round_draw ist deterministisch je Seed und füllt alle Felder", {
  s <- new_tournament_state()
  for (i in 1:8) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  d1 <- generate_round_draw(s, round = 2L, seed = 42L)
  d2 <- generate_round_draw(s, round = 2L, seed = 42L)
  expect_identical(d1$pairings, d2$pairings)
  ids <- unlist(lapply(d1$pairings, function(p) c(p$team1, p$team2)))
  expect_length(unique(ids), 8L)
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-draw_engine.R')"`
Expected: FAIL — `score_draw` fehlt.

- [ ] **Step 3: Implementierung anhängen**

```r
# Gewichte: Hierarchie über Größenordnungen (höhere Prio dominiert immer)
.W_PARTNER <- 1e5; .W_PREV <- 1e3; .W_TEAM <- 1e2; .W_OPP <- 1e1; .W_BALANCE <- 1

score_draw <- function(pairings, histories, ranking) {
  partner <- histories$partner; prev <- histories$prev
  team <- histories$team; opp <- histories$opp
  rank_of <- function(id) { k <- which(ranking$player_id == id)
    if (length(k)) ranking$rank[k] else 9999L }
  pen <- 0; viol <- c(partner = FALSE, prev = FALSE, team = FALSE, opp = FALSE, balance = FALSE)
  tid <- function(x) paste(sort(x), collapse = "|")
  med <- stats::median(ranking$rank)
  for (p in pairings) {
    t1 <- p$team1; t2 <- p$team2
    in_hist <- function(h, a, b) !is.null(h[[as.character(a)]]) && b %in% h[[as.character(a)]]
    # Prio 1: Partner
    if (in_hist(partner, t1[1], t1[2])) { pen <- pen + .W_PARTNER; viol["partner"] <- TRUE }
    if (in_hist(partner, t2[1], t2[2])) { pen <- pen + .W_PARTNER; viol["partner"] <- TRUE }
    # Prio 3: Gegner aus Vorrunde
    for (a in t1) for (b in t2) if (in_hist(prev, a, b)) { pen <- pen + .W_PREV; viol["prev"] <- TRUE }
    # Prio 4: Gegner-Team
    for (a in t1) if (!is.null(team[[as.character(a)]]) && tid(t2) %in% team[[as.character(a)]]) {
      pen <- pen + .W_TEAM; viol["team"] <- TRUE }
    # Prio 5: Einzelgegner
    for (a in t1) for (b in t2) if (in_hist(opp, a, b)) { pen <- pen + .W_OPP; viol["opp"] <- TRUE }
    # Prio 2: stark+schwach (jedes Team soll einen über und einen unter dem Median haben)
    bal_ok <- function(tm) (rank_of(tm[1]) <= med) != (rank_of(tm[2]) <= med)
    if (!bal_ok(t1)) { pen <- pen + .W_BALANCE; viol["balance"] <- TRUE }
    if (!bal_ok(t2)) { pen <- pen + .W_BALANCE; viol["balance"] <- TRUE }
  }
  prio_names <- c(partner = "Keine Partner-Wiederholung", prev = "Neue Gegner vs. Vorrunde",
                  team = "Neue Gegner-Teams", opp = "Neue Einzelgegner",
                  balance = "Stark/Schwach gepaart")
  list(penalty = pen, satisfied = unname(prio_names[!viol]))
}

generate_round_draw <- function(state, round, seed = 1L, n_candidates = 300L) {
  set.seed(seed)
  active_ids <- ts_active_players(state)$player_id
  ranking <- create_ranking(state$games, active_ids)
  # Sicherstellen: jede aktive ID hat einen Rang
  missing <- setdiff(active_ids, ranking$player_id)
  if (length(missing)) ranking <- rbind(ranking[, c("rank","player_id")],
    data.frame(rank = max(c(ranking$rank, 0L)) + seq_along(missing), player_id = missing))
  sel <- select_round_players(state, round, ranking)
  players <- sel$playing
  if (length(players) < 4) return(NULL)
  ranks <- ranking[match(players, ranking$player_id), ]
  ord <- players[order(ranks$rank)]
  mid <- length(ord) %/% 2L
  better_half <- ord[seq_len(mid)]; worse_half <- ord[(mid + 1L):length(ord)]
  histories <- list(
    partner = get_partnership_history(state$games, round),
    prev    = get_previous_round_opponents(state$games, round),
    team    = get_opponent_team_history(state$games, round),
    opp     = get_opponent_history(state$games, round)
  )
  num_fields <- length(players) %/% 4L
  best <- NULL; best_pen <- Inf; best_q <- NULL
  for (i in seq_len(n_candidates)) {
    cand <- generate_candidate(players, better_half, worse_half, num_fields)
    sc <- score_draw(cand, histories, ranking)
    if (sc$penalty < best_pen) { best <- cand; best_pen <- sc$penalty; best_q <- sc$satisfied
      if (best_pen == 0) break }
  }
  list(pairings = best, byes = sel$byes, penalty = best_pen, quality = best_q)
}
```

- [ ] **Step 4: Test laufen lassen (muss bestehen)**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add functions/draw_engine.R tests/testthat/test-draw_engine.R
git commit -m "feat(draw): gewichtetes Scoring und deterministische Auswahl der besten Auslosung"
```

---

## Task 9: Draw-Engine — Härtefälle (Aussetzer, Endlosvermeidung, harte Garantien)

**Files:**
- Modify: `tests/testthat/test-draw_engine.R`
- Modify: `functions/draw_engine.R` (nur falls Tests Lücken zeigen)

**Interfaces:**
- Consumes: `generate_round_draw()`.

- [ ] **Step 1: Härtefall-Tests schreiben**

```r
test_that("18 Spieler / 4 Felder => 16 spielen, 2 setzen aus, alle verschieden", {
  s <- new_tournament_state()
  for (i in 1:18) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 7L, 4L, "best_of_3_11")
  d <- generate_round_draw(s, round = 1L, seed = 7L)
  ids <- unlist(lapply(d$pairings, function(p) c(p$team1, p$team2)))
  expect_length(ids, 16L)
  expect_length(unique(ids), 16L)
  expect_length(d$byes, 2L)
})

test_that("über mehrere Runden bleibt jeder Spieler genau einmal pro Runde", {
  s <- new_tournament_state()
  for (i in 1:8) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  for (rnd in 1:3) {
    d <- generate_round_draw(s, round = rnd, seed = 100L + rnd)
    s <- ts_set_round_games(s, rnd, d$pairings)
    for (gid in s$games$game_id[s$games$round == rnd]) {
      s <- ts_save_result(s, gid, c(11L, 11L), c(5L, 7L))
    }
    s <- ts_lock_round(s, rnd)
    ids <- unlist(lapply(d$pairings, function(p) c(p$team1, p$team2)))
    expect_length(unique(ids), length(ids))
  }
  succeed()
})
```

- [ ] **Step 2: Tests laufen lassen**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS. Falls FAIL → minimal in `generate_candidate`/`generate_round_draw` nachbessern (z. B. Restpool-Auffüllung), bis grün.

- [ ] **Step 3: Commit**

```bash
git add functions/draw_engine.R tests/testthat/test-draw_engine.R
git commit -m "test(draw): Härtefälle Aussetzer und Mehrrunden-Disjunktheit"
```

---

## Task 10: Serialisierung & Schema-Migration (Backup-Format)

**Files:**
- Modify: `functions/tournament_state.R`
- Create: `tests/testthat/test-serialization.R`

**Interfaces:**
- Consumes: `new_tournament_state()`, alle `ts_*`-Mutationen.
- Produces: `state_to_json(state)`, `state_from_json(json)`, `migrate_state(raw)`. Round-Trip muss strukturgleich sein; `migrate_state` hebt `schema_version < 2` auf 2 an.

- [ ] **Step 1: Failing tests schreiben**

`tests/testthat/test-serialization.R`:
```r
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")

test_that("state_to_json -> state_from_json ist verlustfrei (Round-Trip)", {
  s <- new_tournament_state(name = "RT", created_at = "2026-06-27T10:00:00")
  for (i in 1:4) s <- ts_add_player(s, paste("P", i), if (i %% 2) "m" else "w")
  s <- ts_start_tournament(s, 5L, 1L, "best_of_3_11")
  s <- ts_set_round_games(s, 1L, list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L))))
  s <- ts_save_result(s, s$games$game_id[1], c(11L, 11L), c(5L, 7L))
  back <- state_from_json(state_to_json(s))
  expect_equal(back$tournament_name, s$tournament_name)
  expect_equal(back$current_round, s$current_round)
  expect_equal(back$players, s$players)
  expect_equal(back$games$t1_points, s$games$t1_points)
})

test_that("migrate_state hebt alte schema_version an", {
  raw <- list(schema_version = 1L, tournament_name = "Alt",
              settings = list(num_rounds = 5L, num_fields = 4L, game_system = "best_of_3_11"),
              status = "running", current_round = 1L,
              players = empty_players_df(), games = empty_games_df())
  m <- migrate_state(raw)
  expect_equal(m$schema_version, 2L)
})
```

- [ ] **Step 2: Test laufen lassen (muss fehlschlagen)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-serialization.R')"`
Expected: FAIL — `state_to_json` fehlt.

- [ ] **Step 3: Implementierung anhängen** (`functions/tournament_state.R`)

```r
state_to_json <- function(state) {
  jsonlite::toJSON(state, dataframe = "columns", null = "null",
                   na = "null", auto_unbox = TRUE, pretty = TRUE)
}

.as_players_df <- function(x) {
  if (is.null(x) || length(x) == 0) return(empty_players_df())
  data.frame(player_id = as.integer(x$player_id), name = as.character(x$name),
             gender = as.character(x$gender), active = as.logical(x$active),
             stringsAsFactors = FALSE)
}

.as_games_df <- function(x) {
  base <- empty_games_df()
  if (is.null(x) || length(x) == 0) return(base)
  cols <- names(base)
  df <- as.data.frame(lapply(cols, function(cn) {
    v <- x[[cn]]
    if (is.null(v)) return(rep(if (cn == "locked") NA else NA_integer_, length(x[[1]])))
    if (cn == "locked") as.logical(v) else as.integer(v)
  }), stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

migrate_state <- function(raw) {
  raw$players <- .as_players_df(raw$players)
  raw$games   <- .as_games_df(raw$games)
  raw$current_round <- as.integer(raw$current_round)
  raw$settings$num_rounds <- as.integer(raw$settings$num_rounds)
  raw$settings$num_fields <- as.integer(raw$settings$num_fields)
  raw$schema_version <- SCHEMA_VERSION
  raw
}

state_from_json <- function(json) {
  raw <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  migrate_state(raw)
}
```

- [ ] **Step 4: jsonlite sicherstellen, Tests laufen lassen**

Run: `Rscript -e "renv::install('jsonlite'); renv::snapshot()"`
Run: `Rscript -e "testthat::test_dir('tests/testthat')"`
Expected: PASS (alle Suites grün).

- [ ] **Step 5: Commit**

```bash
git add functions/tournament_state.R tests/testthat/test-serialization.R renv.lock
git commit -m "feat(state): JSON-Serialisierung und Schema-Migration für Backups"
```

---

## Abschluss Phase 1

- [ ] **Vollständiger Testlauf:** `Rscript -e "testthat::test_dir('tests/testthat')"` → alle grün.
- [ ] **Verifikation:** kurzer manueller Smoke-Test-Skript (`tests/smoke_phase1.R`), das ein 16-Spieler-Turnier über 5 Runden simuliert (Auslosung → Ergebnisse → Rangliste → JSON-Round-Trip) und die Ausgabe druckt.
- [ ] Phase 1 fertig → **Phase 2 planen** (Persistenz-JS-Bridge, Backup-Download/Upload, `module_setup`/`module_matchday`/`module_ranking`, UI-Flow, gesperrte Runden, Sieger-Ansicht).

## Self-Review-Notiz (Plan-Autor)

- **Spec-Abdeckung:** §4 State/Serialisierung → Tasks 1–3,10; §5 Algorithmus → Tasks 6–9; §6 Validierung → Task 4; Rangliste → Task 5. UI (§7), JS-localStorage & Backup-Datei (§4.2/4.3) bewusst in **Phase 2** ausgelagert.
- **Reihenfolge-Abhängigkeit:** Task 3 nutzt `sets_won_from_scores` aus Task 4 → Task 4 direkt nach Task 3 ausführen (im Plan vermerkt).
- **Typkonsistenz:** Spalten-/Funktionsnamen über alle Tasks gegen den Schnittstellen-Überblick geprüft.

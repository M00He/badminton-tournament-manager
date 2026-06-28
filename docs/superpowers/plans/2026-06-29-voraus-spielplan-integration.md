# Voraus-Spielplan-Integration (Plan B: State + UI) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Den fertigen Spielplan-Generator (`functions/schedule_planner.R`, Plan A) in die App verdrahten: ein **Modus-Umschalter** (Voraus-Plan / Rundenweise), die Längen-Auswahl im Setup, und der Spieltag-Plan-Modus, der pro Runde eine garantiert-gültige, an die Tabelle re-optimierte Runde vorschlägt.

**Architecture:** `schedule_mode` + `plan_field_sequence` werden Turnier-Einstellungen (in `ts_start_tournament`, persistiert). Der **Plan wird NICHT gespeichert**, sondern bei Bedarf aus dem gespielten Präfix neu erzeugt (= der Existenzbeweis) und an die aktuelle Tabelle re-optimiert — neue Bridge-Funktionen in `functions/plan_integration.R` übersetzen zwischen App-State und Planner. Setup und Spieltag verzweigen auf den Modus; die bestehende Vorschau/Übernehmen-Mechanik wird wiederverwendet.

**Tech Stack:** R, Shiny, bslib; `testthat` + `shiny::testServer`. Pure base R im Kern. Tests über PowerShell (`Rscript`).

## Global Constraints

- **Pure R / keine neuen Paket-Abhängigkeiten** (base R + vorhandene shiny/bslib/jsonlite).
- **`schedule_mode` ∈ {"plan", "round_by_round"}**; Default-AUSWAHL in der UI = `"plan"`; Default in `ts_start_tournament`/`migrate_state` = `"round_by_round"` (Rückwärtskompatibilität für bestehende Tests & alte Backups).
- **Planner-Datenformat (aus Plan A, verbindlich):** eine Runde = `list(field_count, games, byes)`; jedes `games`-Element = `list(field, team1 = c(id,id), team2 = c(id,id))`. Identisch zum `pairings`-Format von `ts_set_round_games`.
- **Plan wird NICHT persistiert** — nur `settings$schedule_mode` + `settings$plan_field_sequence` (int-Vektor) überleben Reload; der konkrete Plan wird regeneriert.
- **Harte Garantien (H1/H2) kommen aus dem Planner** — die UI darf sie nie umgehen: in `"plan"`-Modus stammen Runden ≥ 2 ausschließlich aus `generate_schedule`/`reoptimize_tail`.
- **Runde 1 bleibt manuell** in beiden Modi; in `"plan"`-Modus mit `field_sequence[1]` Feldern.
- **Bestehende Kernfunktionen NICHT entfernen/umbauen** — `ts_start_tournament` etc. nur additiv erweitern (neue optionale Parameter mit rückwärtskompatiblen Defaults).
- **Sprache:** deutsche UI-Strings, konsistent mit der App. `showNotification`-Typen nur `"default"/"message"/"warning"/"error"` (kein `"success"`).
- **Tests:** testthat-Muster wie bestehend (`source("../../functions/...", encoding = "UTF-8")`); App-Build-Smoke `Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj'))"`. Volle Suite `Rscript -e "testthat::test_dir('tests/testthat')"`.
- **Plan-Modus setzt einen stabilen aktiven Spielerkreis voraus** (kein Aussteigen mitten im Turnier); Dropout ist eine dokumentierte Grenze (Spec §11), kein Scope hier.

---

### Task 1: State — `schedule_mode` + `plan_field_sequence`

**Files:**
- Modify: `functions/tournament_state.R` (`new_tournament_state`, `ts_start_tournament`, `migrate_state`)
- Test: `tests/testthat/test-tournament-state-plan.R` (neu)

**Interfaces:**
- Consumes: nichts Neues.
- Produces:
  - `ts_start_tournament(state, num_rounds, num_fields, game_system, tiebreaker_order = "diff_first", schedule_mode = "round_by_round", plan_field_sequence = NULL)` — bei `schedule_mode == "plan"` wird `plan_field_sequence` (int-Vektor, Länge ≥ 1) verlangt, `num_rounds := length(fs)`, `num_fields := max(fs)`, und in `settings` abgelegt.
  - `settings` enthält nun stets `schedule_mode`; bei Plan-Modus zusätzlich `plan_field_sequence` (int-Vektor), sonst `plan_field_sequence = NULL`.
  - `migrate_state` defaultet fehlendes `schedule_mode` auf `"round_by_round"` und coerced `plan_field_sequence` nach integer.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-tournament-state-plan.R`:

```r
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")

mk_players <- function(n) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(n)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s
}

test_that("ts_start_tournament Plan-Modus speichert Felder-Folge und leitet Runden/Felder ab", {
  s <- mk_players(8)
  fs <- c(2L, 2L, 2L, 1L, 1L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)
  expect_equal(s$settings$schedule_mode, "plan")
  expect_equal(s$settings$plan_field_sequence, fs)
  expect_equal(s$settings$num_rounds, 5L)        # = length(fs), num_rounds-Arg ignoriert
  expect_equal(s$settings$num_fields, 2L)         # = max(fs)
  expect_equal(s$status, "running")
})

test_that("ts_start_tournament Plan-Modus ohne Felder-Folge ist ein Fehler", {
  s <- mk_players(8)
  expect_error(ts_start_tournament(s, 5L, 2L, "best_of_3_11", "diff_first",
                                   schedule_mode = "plan", plan_field_sequence = NULL))
})

test_that("ts_start_tournament Default bleibt round_by_round (rueckwaertskompatibel)", {
  s <- mk_players(8)
  s <- ts_start_tournament(s, 5L, 2L, "best_of_3_11")
  expect_equal(s$settings$schedule_mode, "round_by_round")
  expect_null(s$settings$plan_field_sequence)
  expect_equal(s$settings$num_rounds, 5L)
})

test_that("migrate_state: altes Backup ohne schedule_mode -> round_by_round; Plan-Folge -> integer", {
  raw_old <- list(schema_version = 2L, tournament_name = "X", created_at = "",
                  settings = list(num_rounds = 5, num_fields = 2, game_system = "best_of_3_11",
                                  tiebreaker_order = "diff_first"),
                  status = "running", current_round = 1, players = NULL, games = NULL)
  m <- migrate_state(raw_old)
  expect_equal(m$settings$schedule_mode, "round_by_round")

  raw_plan <- raw_old
  raw_plan$settings$schedule_mode <- "plan"
  raw_plan$settings$plan_field_sequence <- c(2, 2, 2, 1, 1)   # via JSON kommen Doubles
  m2 <- migrate_state(raw_plan)
  expect_type(m2$settings$plan_field_sequence, "integer")
  expect_equal(m2$settings$plan_field_sequence, c(2L,2L,2L,1L,1L))
})

test_that("Serialisierung erhaelt schedule_mode + plan_field_sequence (Round-Trip)", {
  s <- ts_start_tournament(mk_players(8), 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = c(2L,2L,2L,1L,1L))
  back <- state_from_json(state_to_json(s))
  expect_equal(back$settings$schedule_mode, "plan")
  expect_equal(back$settings$plan_field_sequence, c(2L,2L,2L,1L,1L))
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run (PowerShell, Repo-Root): `Rscript -e "testthat::test_file('tests/testthat/test-tournament-state-plan.R')"`
Expected: FEHLER (Plan-Modus-Argumente noch nicht unterstützt; `schedule_mode` fehlt in settings).

- [ ] **Step 3: `new_tournament_state` Default-Settings ergänzen**

In `functions/tournament_state.R`, in `new_tournament_state`, die `settings`-Liste erweitern (Zeilen ~33-35):

```r
    settings        = list(num_rounds = 5L, num_fields = 4L,
                           game_system = "best_of_3_11",
                           tiebreaker_order = "diff_first",
                           schedule_mode = "round_by_round"),
```

- [ ] **Step 4: `ts_start_tournament` erweitern**

Ersetze die gesamte `ts_start_tournament`-Funktion durch:

```r
ts_start_tournament <- function(state, num_rounds, num_fields, game_system,
                                tiebreaker_order = "diff_first",
                                schedule_mode = "round_by_round",
                                plan_field_sequence = NULL) {
  if (nrow(ts_active_players(state)) < 4) stop("Mindestens 4 aktive Spieler benötigt.")
  stopifnot(tiebreaker_order %in% c("diff_first", "direct_first"))
  stopifnot(schedule_mode %in% c("plan", "round_by_round"))
  if (identical(schedule_mode, "plan")) {
    if (is.null(plan_field_sequence) || length(plan_field_sequence) == 0)
      stop("Voraus-Plan benötigt eine Felder-Folge.")
    plan_field_sequence <- as.integer(plan_field_sequence)
    num_rounds <- length(plan_field_sequence)
    num_fields <- max(plan_field_sequence)
  } else {
    plan_field_sequence <- NULL
  }
  state$settings <- list(num_rounds = as.integer(num_rounds),
                         num_fields = as.integer(num_fields),
                         game_system = game_system,
                         tiebreaker_order = tiebreaker_order,
                         schedule_mode = schedule_mode,
                         plan_field_sequence = plan_field_sequence)
  state$current_round <- 1L
  state$status <- "running"
  state$games <- empty_games_df()
  state
}
```

- [ ] **Step 5: `migrate_state` erweitern**

In `functions/tournament_state.R`, in `migrate_state`, direkt vor `raw$schema_version <- SCHEMA_VERSION` einfügen:

```r
  if (is.null(raw$settings$schedule_mode)) raw$settings$schedule_mode <- "round_by_round"
  if (!is.null(raw$settings$plan_field_sequence))
    raw$settings$plan_field_sequence <- as.integer(raw$settings$plan_field_sequence)
```

- [ ] **Step 6: Tests laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-tournament-state-plan.R')"` → PASS.
Dann volle Suite: `Rscript -e "testthat::test_dir('tests/testthat')"` → alle grün (bestehende Tests unverändert, da Default round_by_round).

- [ ] **Step 7: Commit**

```bash
git add functions/tournament_state.R tests/testthat/test-tournament-state-plan.R
git commit -m "feat(state): schedule_mode + plan_field_sequence in ts_start_tournament/migrate"
```

---

### Task 2: Bridge `functions/plan_integration.R`

**Files:**
- Create: `functions/plan_integration.R`
- Test: `tests/testthat/test-plan-integration.R` (neu)

**Interfaces:**
- Consumes: `schedule_planner.R` (`generate_schedule`, `reoptimize_tail`, `verify_schedule`), `ranking_calculation.R` (`create_ranking`), `tournament_state.R` (`ts_active_players`).
- Produces:
  - `games_round_to_plan(state, round) -> list(field_count, games, byes) | NULL` — eine gespielte Runde (aus `state$games`) ins Planner-Format.
  - `played_rounds_as_plan(state) -> list` — alle Runden mit Spielen, aufsteigend, im Planner-Format (= locked-prefix).
  - `strength_from_ranking(state) -> named numeric` (names = player_id-String; höher = stärker), aus der aktuellen Rangliste.
  - `plan_next_round_pairings(state, seed = 1L, n_candidates = 300L) -> list(pairings, byes) | NULL` — erzeugt eine garantiert-gültige, an die Tabelle re-optimierte Fortsetzung und gibt die Runde `current_round` im Matchday-`pairings`-Format zurück.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-plan-integration.R`:

```r
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/plan_integration.R", encoding = "UTF-8")

# 8 Spieler, 2 Felder, 5 Runden Plan-Modus; Runde 1 gespielt + Ergebnisse.
mk_started_plan <- function() {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(8L, 2L, 5L)            # c(2,2,2,1,1), G=4
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
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration.R')"`
Expected: FEHLER — `could not find function "games_round_to_plan"`.

- [ ] **Step 3: `functions/plan_integration.R` implementieren**

```r
# Bruecke zwischen App-State und Spielplan-Generator (schedule_planner.R).
# Uebersetzt gespielte Runden in das Planner-Format und erzeugt die naechste
# garantiert-gueltige, an die aktuelle Tabelle re-optimierte Runde.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Eine gespielte Runde (aus state$games) ins Planner-Format list(field_count, games, byes).
games_round_to_plan <- function(state, round) {
  g <- state$games[state$games$round == round, , drop = FALSE]
  if (nrow(g) == 0) return(NULL)
  active <- ts_active_players(state)$player_id
  played <- c(g$t1_p1, g$t1_p2, g$t2_p1, g$t2_p2)
  byes <- as.integer(setdiff(active, played))
  games <- lapply(seq_len(nrow(g)), function(i) {
    x <- g[i, ]
    list(field = as.integer(x$field),
         team1 = c(as.integer(x$t1_p1), as.integer(x$t1_p2)),
         team2 = c(as.integer(x$t2_p1), as.integer(x$t2_p2)))
  })
  list(field_count = nrow(g), games = games, byes = byes)
}

# Alle Runden mit Spielen, aufsteigend = locked-prefix fuer den Generator.
played_rounds_as_plan <- function(state) {
  rounds <- sort(unique(state$games$round))
  out <- lapply(rounds, function(r) games_round_to_plan(state, r))
  out[!vapply(out, is.null, logical(1))]
}

# Staerke je Spieler aus der aktuellen Rangliste (hoeher = staerker).
strength_from_ranking <- function(state) {
  ids <- ts_active_players(state)$player_id
  if (length(ids) == 0) return(setNames(numeric(0), character(0)))
  r <- create_ranking(state$games, ids, state$settings$tiebreaker_order %||% "diff_first")
  n <- nrow(r)
  setNames(as.numeric(n - r$rank + 1L), as.character(r$player_id))  # Rang 1 -> hoechste Staerke
}

# Naechste Runde (= current_round) als garantiert-gueltige, an die Tabelle re-optimierte
# Fortsetzung des gespielten Praefix. Rueckgabe im Matchday-pairings-Format oder NULL.
plan_next_round_pairings <- function(state, seed = 1L, n_candidates = 300L) {
  fs <- state$settings$plan_field_sequence
  if (is.null(fs) || length(fs) == 0) return(NULL)
  players <- ts_active_players(state)$player_id
  k <- state$current_round
  if (k > length(fs)) return(NULL)
  played <- played_rounds_as_plan(state)
  base <- generate_schedule(players, fs, locked_rounds = played, seed = seed)
  if (is.null(base)) return(NULL)
  strength <- strength_from_ranking(state)
  full <- reoptimize_tail(players, fs, played_rounds = played, strength = strength,
                          current_schedule = base, n_candidates = n_candidates, seed = seed)
  rd <- full[[k]]
  if (is.null(rd)) return(NULL)
  list(pairings = rd$games, byes = rd$byes)
}
```

- [ ] **Step 4: Tests laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-plan-integration.R')"` → PASS (5 Tests grün).

- [ ] **Step 5: Commit**

```bash
git add functions/plan_integration.R tests/testthat/test-plan-integration.R
git commit -m "feat(plan-integration): Bruecke State<->Generator + naechste Plan-Runde"
```

---

### Task 3: Setup — Modus- & Längen-Auswahl

**Files:**
- Modify: `modules/module_setup.R` (`module_setup_ui` Einstellungs-Card, `module_setup_server` start-Logik + plan_rounds-Observer)
- Test: `tests/testthat/test-module-setup-plan.R` (neu)

**Interfaces:**
- Consumes: `plan_options`, `field_sequence_for`, `default_plan_rounds` (Planner), `ts_start_tournament` (Task 1), `ts_active_players`.
- Produces: keine neue exportierte Funktion; das Setup-Modul setzt bei Start in `state_rv()$settings` `schedule_mode` + (im Plan-Modus) `plan_field_sequence`.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-module-setup-plan.R`:

```r
source("../../functions/game_system.R", encoding = "UTF-8")
source("../../functions/ranking_calculation.R", encoding = "UTF-8")
source("../../functions/tournament_state.R", encoding = "UTF-8")
source("../../functions/schedule_planner.R", encoding = "UTF-8")
source("../../functions/app_helpers.R", encoding = "UTF-8")
source("../../modules/module_setup.R", encoding = "UTF-8")
library(shiny)

mk_players <- function(n) {
  s <- new_tournament_state(name = "T")
  for (i in seq_len(n)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  s
}

test_that("module_setup: Plan-Modus startet Turnier mit Felder-Folge", {
  rv <- reactiveVal(mk_players(14))
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(schedule_mode = "plan", num_fields = 3,
                      game_system = "best_of_3_11", tiebreaker = "diff_first")
    session$setInputs(plan_rounds = "7")            # 14/3/7 -> G=6
    session$setInputs(start = 1)
    s <- rv()
    expect_equal(s$status, "running")
    expect_equal(s$settings$schedule_mode, "plan")
    expect_equal(length(s$settings$plan_field_sequence), 7L)
    expect_equal(s$settings$num_rounds, 7L)
  })
})

test_that("module_setup: Rundenweise-Modus startet wie bisher", {
  rv <- reactiveVal(mk_players(8))
  testServer(module_setup_server, args = list(state_rv = rv), {
    session$setInputs(schedule_mode = "round_by_round", num_rounds = 5, num_fields = 2,
                      game_system = "best_of_3_11", tiebreaker = "diff_first")
    session$setInputs(start = 1)
    s <- rv()
    expect_equal(s$status, "running")
    expect_equal(s$settings$schedule_mode, "round_by_round")
    expect_equal(s$settings$num_rounds, 5L)
    expect_null(s$settings$plan_field_sequence)
  })
})
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-setup-plan.R')"`
Expected: FEHLER (kein `schedule_mode`-Input; start ignoriert Plan-Felder).

- [ ] **Step 3: Einstellungs-Card im UI ersetzen**

In `modules/module_setup.R`, die zweite `card(...)` (ab `card_header("Einstellungen")`, Zeilen ~22-34) ersetzen durch:

```r
    card(
      card_header("Einstellungen"),
      radioButtons(ns("schedule_mode"), "Spielplan-Modus:",
        c("Voraus-Plan — gleiche Spiele & keine Partner-Wiederholung (garantiert)" = "plan",
          "Rundenweise — frei pro Runde (wie gehabt)" = "round_by_round"),
        selected = "plan"),
      numericInput(ns("num_fields"), "Anzahl Felder (Plätze):", 4, min = 1, max = 10),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'plan'", ns("schedule_mode")),
        selectInput(ns("plan_rounds"), "Anzahl Runden:", choices = NULL),
        uiOutput(ns("plan_info"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'round_by_round'", ns("schedule_mode")),
        numericInput(ns("num_rounds"), "Anzahl Runden:", 5, min = 1, max = 20)
      ),
      selectInput(ns("game_system"), "Spielsystem:", c(
        "Zwei Gewinnsätze bis 11" = "best_of_3_11", "Ein Satz bis 15" = "single_15",
        "Ein Satz bis 21" = "single_21", "Ein Satz bis 30" = "single_30")),
      selectInput(ns("tiebreaker"), "Bei Gleichstand zuerst:", c(
        "Punktedifferenz" = "diff_first", "Direkter Vergleich" = "direct_first")),
      br(),
      actionButton(ns("start"), "Turnier starten", class = "btn-success btn-lg", icon = icon("play")),
      uiOutput(ns("status"))
    )
```

- [ ] **Step 4: Server — plan_rounds-Observer + plan_info + start-Logik**

In `modules/module_setup.R`, im `module_setup_server` direkt nach `ns <- session$ns` (Zeile ~40) einfügen:

```r
    # Rundenzahl-Optionen aus aktiver Spielerzahl + Felderzahl ableiten (Plan-Modus)
    observe({
      P <- nrow(ts_active_players(state_rv())); Fm <- input$num_fields
      if (is.null(Fm) || is.na(Fm) || P < 4 || Fm < 1) {
        updateSelectInput(session, "plan_rounds", choices = character(0)); return()
      }
      opts <- plan_options(as.integer(P), as.integer(Fm))
      if (length(opts) == 0) {
        updateSelectInput(session, "plan_rounds", choices = character(0)); return()
      }
      labels <- vapply(opts, function(o) sprintf("%d Runden  (je %d Spiele, %d× Pause)",
                                                 o$rounds, o$games, o$byes), "")
      vals <- vapply(opts, function(o) as.character(o$rounds), "")
      def <- default_plan_rounds(as.integer(P), as.integer(Fm))
      updateSelectInput(session, "plan_rounds", choices = setNames(vals, labels),
                        selected = as.character(def))
    })

    output$plan_info <- renderUI({
      P <- nrow(ts_active_players(state_rv())); Fm <- input$num_fields
      R <- suppressWarnings(as.integer(input$plan_rounds))
      if (is.null(Fm) || is.na(Fm) || P < 4 || is.na(R)) return(em("Spieler & Felder wählen."))
      fs <- field_sequence_for(as.integer(P), as.integer(Fm), R)
      if (is.null(fs)) return(em("Für diese Kombination gibt es keinen gültigen Plan."))
      G <- sum(4L * fs) %/% P
      div(style = "color:#555;margin-top:4px;",
        sprintf("%d Spieler · jeder %d Spiele · %d× Pause · keine Partner-Wiederholung.",
                P, G, R - G))
    })
```

Danach `start_now` (die bestehende Funktion, Zeilen ~92-98) ersetzen durch:

```r
    start_now <- function() {
      s <- state_rv(); mode <- input$schedule_mode %||% "plan"
      tryCatch({
        if (identical(mode, "plan")) {
          P <- nrow(ts_active_players(s)); Fm <- as.integer(input$num_fields)
          R <- suppressWarnings(as.integer(input$plan_rounds))
          if (is.na(R)) stop("Bitte eine Rundenzahl wählen.")
          fs <- field_sequence_for(P, Fm, R)
          if (is.null(fs)) stop("Für diese Kombination gibt es keinen gültigen Plan.")
          state_rv(ts_start_tournament(state_rv(), R, Fm, input$game_system, input$tiebreaker,
                                       schedule_mode = "plan", plan_field_sequence = fs))
        } else {
          state_rv(ts_start_tournament(state_rv(), input$num_rounds, input$num_fields,
                                       input$game_system, input$tiebreaker,
                                       schedule_mode = "round_by_round"))
        }
        showNotification("Turnier gestartet! Weiter zum Spieltag.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    }
```

- [ ] **Step 5: Tests laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-setup-plan.R')"` → PASS.
Bestehende Setup-Tests: `Rscript -e "testthat::test_file('tests/testthat/test-module-setup.R')"` → weiterhin grün (falls vorhanden; sonst überspringen).

- [ ] **Step 6: Commit**

```bash
git add modules/module_setup.R tests/testthat/test-module-setup-plan.R
git commit -m "feat(setup): Modus-Umschalter + Plan-Laengenauswahl (Runden aus plan_options)"
```

---

### Task 4: Spieltag — Plan-Modus-Zweig

**Files:**
- Modify: `modules/module_matchday.R` (`round_fields` reactive, `output$header` field_picker + Steuer-Buttons, `do_preview`)
- Test: `tests/testthat/test-module-matchday-plan.R` (neu)

**Interfaces:**
- Consumes: `plan_next_round_pairings` (Task 2), `state$settings$schedule_mode`/`plan_field_sequence` (Task 1).
- Produces: keine neue Funktion; im Plan-Modus stammen Runden ≥ 2 aus dem Generator, die Felderzahl pro Runde ist durch `plan_field_sequence` fixiert.

- [ ] **Step 1: Failing test schreiben**

Erstelle `tests/testthat/test-module-matchday-plan.R`:

```r
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
```

- [ ] **Step 2: Test laufen lassen, Fehlschlag prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday-plan.R')"`
Expected: FEHLER — der Plan-Modus-Vorschlag fehlt (do_preview nutzt nur generate_round_draw; round_fields ignoriert plan_field_sequence).

- [ ] **Step 3: `round_fields` plan-bewusst machen**

In `modules/module_matchday.R` die `round_fields`-reactive (Zeilen ~24-28) ersetzen durch:

```r
    # Felder für DIESE Runde. Im Plan-Modus durch die Felder-Folge fixiert,
    # sonst Einstellung (pro Runde reduzierbar über den Picker).
    round_fields <- reactive({
      s <- state_rv()
      if (identical(s$settings$schedule_mode, "plan") && !is.null(s$settings$plan_field_sequence)) {
        fs <- s$settings$plan_field_sequence
        r <- s$current_round
        if (r >= 1 && r <= length(fs)) return(as.integer(fs[r]))
      }
      dflt <- s$settings$num_fields
      nf <- input$round_fields
      if (is.null(nf) || is.na(nf) || nf < 1) dflt else min(as.integer(nf), dflt)
    })
```

- [ ] **Step 4: Header — Picker im Plan-Modus verstecken, Buttons umbenennen**

In `modules/module_matchday.R`, in `output$header` (Zeilen ~41-64), die Block-Definitionen für `controls` und `field_picker` ersetzen durch:

```r
      plan_mode <- identical(s$settings$schedule_mode, "plan")
      controls <- if (!has_games && s$current_round >= 2) {
        if (plan_mode)
          tagList(
            actionButton(ns("preview"), "Geplante Runde anzeigen", class = "btn-primary"),
            actionButton(ns("reroll"), "Anders planen", class = "btn-info"))
        else
          tagList(
            actionButton(ns("preview"), "Auslosung vorschlagen", class = "btn-primary"),
            actionButton(ns("reroll"), "Neu würfeln", class = "btn-info"))
      } else NULL
      field_picker <- if (!has_games && !plan_mode && s$settings$num_fields > 1)
        numericInput(ns("round_fields"), "Felder diese Runde:", value = s$settings$num_fields,
                     min = 1, max = s$settings$num_fields, width = "150px") else NULL
```

- [ ] **Step 5: `do_preview` verzweigen**

In `modules/module_matchday.R` die `do_preview`-Funktion (Zeilen ~123-129) ersetzen durch:

```r
    do_preview <- function() {
      s <- state_rv()
      if (identical(s$settings$schedule_mode, "plan")) {
        d <- plan_next_round_pairings(s, seed = seed_rv(), n_candidates = 300L)
        if (is.null(d)) {
          showNotification("Plan: keine gültige Fortsetzung gefunden.", type = "error"); return()
        }
        d$quality <- "gleiche Spielzahl + keine Partner-Wiederholung (garantiert)"
        preview_rv(d)
      } else {
        d <- generate_round_draw(s, s$current_round, seed = seed_rv(), n_candidates = 300L,
                                 n_fields = round_fields())
        if (is.null(d)) {
          showNotification("Keine Auslosung möglich (zu wenige Spieler).", type = "error"); return()
        }
        preview_rv(d)
      }
    }
```

- [ ] **Step 6: Tests laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday-plan.R')"` → PASS.
Bestehende Matchday-Tests: `Rscript -e "testthat::test_file('tests/testthat/test-module-matchday.R')"` → weiterhin grün (Rundenweise-Pfad unverändert; `round_fields` für round_by_round unverändert).

- [ ] **Step 7: Commit**

```bash
git add modules/module_matchday.R tests/testthat/test-module-matchday-plan.R
git commit -m "feat(matchday): Plan-Modus — fixe Felder je Runde + Generator-Vorschlag"
```

---

### Task 5: End-to-End Plan-Modus + App-Build

**Files:**
- Test: `tests/testthat/test-e2e-plan.R` (neu)

**Interfaces:**
- Consumes: alles aus Task 1-4 + Kern.
- Produces: nichts (Integrationsnachweis).

- [ ] **Step 1: Headless-Vollturnier-Test schreiben**

Erstelle `tests/testthat/test-e2e-plan.R`:

```r
for (f in list.files("../../functions", pattern = "[.]R$", full.names = TRUE))
  source(f, encoding = "UTF-8")

test_that("E2E: Plan-Modus 8 Spieler / 2 Felder / 5 Runden — gleiche Spiele, keine Partner-Wiederholung", {
  s <- new_tournament_state(name = "E2E")
  for (i in seq_len(8)) s <- ts_add_player(s, paste("Spieler", i), if (i %% 2) "m" else "w")
  fs <- field_sequence_for(8L, 2L, 5L)
  s <- ts_start_tournament(s, 99L, 99L, "best_of_3_11", "diff_first",
                           schedule_mode = "plan", plan_field_sequence = fs)

  # Runde 1 manuell
  r1 <- list(list(field = 1L, team1 = c(1L,2L), team2 = c(3L,4L)),
             list(field = 2L, team1 = c(5L,6L), team2 = c(7L,8L)))
  s <- ts_set_round_games(s, 1L, r1)

  # Runden bis Ende durchspielen: Ergebnisse eintragen, sperren, Plan-Runde erzeugen, übernehmen
  repeat {
    rnd <- s$current_round
    for (gid in s$games$game_id[s$games$round == rnd]) {
      s <- ts_save_result(s, gid, c(11L, 11L, NA), c(5L, 7L, NA))
    }
    s <- ts_lock_round(s, rnd)
    s <- ts_advance_round(s)
    if (s$status == "finished") break
    d <- plan_next_round_pairings(s, seed = rnd, n_candidates = 80L)
    expect_false(is.null(d), info = sprintf("Runde %d: kein Plan", s$current_round))
    s <- ts_set_round_games(s, s$current_round, d$pairings)
  }

  # Gesamtplan aus state$games verifizieren
  full <- played_rounds_as_plan(s)
  expect_equal(length(full), 5L)
  v <- verify_schedule(full, 1:8)
  expect_true(v$ok, info = paste(v$errors, collapse = "; "))
  expect_equal(length(v$partner_repeats), 0L)
  expect_true(v$equal_games)
  expect_equal(unname(v$games_per_player["1"]), 4L)   # G = 4
})
```

- [ ] **Step 2: Test laufen lassen, Erfolg prüfen**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-e2e-plan.R')"` → PASS (gleiche Spiele, keine Partner-Wiederholung über das volle Turnier).
Falls ein `plan_next_round_pairings` NULL liefert oder `verify_schedule` fehlschlägt: NICHT die Erwartung abschwächen — STOPP und melden (es wäre ein echter Integrationsfehler).

- [ ] **Step 3: App-Build-Smoke + volle Suite**

Run (App lädt alle Module + die neue Datei sauber):
`Rscript -e "a <- source('app.R')$value; stopifnot(inherits(a,'shiny.appobj')); cat('APP OK\n')"`
Expected: `APP OK`.

Run volle Suite: `Rscript -e "testthat::test_dir('tests/testthat')"` → alles grün.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-e2e-plan.R
git commit -m "test(e2e): Plan-Modus Vollturnier (gleiche Spiele + keine Partner-Wiederholung) + App-Build"
```

---

## Self-Review

**1. Spec-Abdeckung (gegen Spec §6/§7):**
- §6 `settings$schedule_mode` + Default + Migration → Task 1. ✓
- §6 `settings$plan_field_sequence` (Längen-Repräsentation) → Task 1. ✓
- §6 „Plan wird NICHT persistiert, sondern regeneriert" → Task 2 (`plan_next_round_pairings` regeneriert), bewusste Vereinfachung ggü. Spec-Wortlaut „state$plan" — der konkrete Plan ist regenerierbar; die Garantie bleibt. ✓ (in Spec §4 gedeckt: „neu gerechnet bei Abweichung")
- §7 Setup Modus-Auswahl + Längen-Auswahl aus `plan_options` → Task 3. ✓
- §7 Spieltag Plan-Modus: geplante Runde zeigen/übernehmen, fixe Felder, „anders planen" (reroll) → Task 4. ✓
- §7 „manuelle Eingriffe bleiben": die bestehenden Feld-/Spieler-Edits + Ergebnis-Handler in `output$fields`/`save_game` sind unverändert → bleiben in beiden Modi nutzbar (kein Task nötig, da nicht angefasst). ✓
- §4 Re-Optimierung nach jeder Runde gegen die Tabelle → Task 2 (`reoptimize_tail` mit `strength_from_ranking`), pro Runde via `do_preview`. ✓
- §10 Tests: Property/Integration → Task 2 + Task 5 (E2E). ✓

**Bewusste Vereinfachung ggü. Spec §6:** `state$plan` wird NICHT als Feld gespeichert/serialisiert. Stattdessen Regeneration aus dem gespielten Präfix (Task 2). Begründung: der konkrete Restplan ist regenerierbar und wird ohnehin nach jeder Runde re-optimiert; das spart fehleranfällige Verschachtelt-Listen-Serialisierung. Die harte Garantie (immer existiert eine gültige Fortsetzung) bleibt voll erhalten. Falls später „kommende Runden vorab anzeigen" gewünscht wird, kann `state$plan` additiv ergänzt werden.

**2. Platzhalter-Scan:** kein TBD/TODO; jeder Code-Step vollständig. ✓

**3. Typ-Konsistenz:** `schedule_mode`/`plan_field_sequence` einheitlich in Task 1/2/3/4; Runden-Format `list(field_count, games=list(list(field,team1,team2)), byes)` durchgehend; `plan_next_round_pairings` liefert `list(pairings, byes)` wie von `do_preview`/`accept`/`preview_box` erwartet. ✓

**Offene Grenze (dokumentiert):** Spieler-Dropout mitten im Plan-Turnier ändert P und damit die Gültigkeit der `plan_field_sequence` → `plan_next_round_pairings` liefert dann ggf. NULL („keine gültige Fortsetzung"). Manuelle Eingriffe (Spieler tauschen, Felder im Rundenweise-Modus) bleiben als Notnagel. Voller Dropout-Support ist Spec §11 (out of scope).
